package CfDebugAdapter

import scala.jdk.CollectionConverters._;
import scala.annotation.tailrec;
import scala.annotation.targetName;
import scala.collection.{mutable, immutable}

import com.sun.jdi.*;
import com.sun.jdi.request.{ClassUnloadRequest, MethodExitRequest, EventRequest, StepRequest, ClassPrepareRequest, ThreadStartRequest, ThreadDeathRequest, BreakpointRequest => jdi_BreakpointRequest};
import com.sun.jdi.event.*;

import java.util.ArrayList
import java.io.OutputStream
import java.nio.file.Path;
import java.nio.file.Files;
import scala.util.Success
import scala.util.Try
import scala.util.Failure
import org.eclipse.lsp4j.debug.{SourceBreakpoint => lsp4j_SourceBreakpoint}
import scala.collection.mutable.ArrayBuffer
import org.eclipse.lsp4j.debug.services.IDebugProtocolClient
import org.eclipse.lsp4j.debug.{Source => lsp4j_Source};
import org.eclipse.lsp4j.debug.StoppedEventArguments
import org.eclipse.lsp4j.debug.StoppedEventArgumentsReason
import org.eclipse.lsp4j.debug.StackTraceArguments
import org.eclipse.lsp4j.debug.StackTraceResponse
import org.eclipse.lsp4j.debug.{StackFrame => lsp4j_StackFrame}
import scala.languageFeature.existentials
import java.io.File

object SourceManager {
    enum AddOrGetResult(val newest: CfSourceFileWrapper) {
        case NoChange(current: CfSourceFileWrapper) extends AddOrGetResult(newest = current);
        case Replace(before: CfSourceFileWrapper, after: CfSourceFileWrapper) extends AddOrGetResult(newest = after);
        case Add(fresh: CfSourceFileWrapper) extends AddOrGetResult(newest = fresh);
    }

}
class SourceManager() {
    import SourceManager.*;

    // from the client/IDE, we will get CFM file paths representing a CF file
    private val pathToRef = mutable.Map[String, CfSourceFileWrapper]();
    // from the jvm, we will get `ReferenceType` objects representing a CF file in its compiled-to-classfile form
    private val refSet = mutable.Map[ReferenceType, CfSourceFileWrapper]();

    // we only ever add source files if they are live on the jvm
    // so there is no (string, string) overload

    def addOrGet(refType: ReferenceType, absPath: String) : AddOrGetResult = {
        import AddOrGetResult.*;

        val fresh = CfSourceFileWrapper(absPath.toString(), refType);

        def push() : CfSourceFileWrapper = {
            pathToRef.put(fresh.absPath, fresh);
            refSet.put(fresh.refType, fresh);
            fresh;
        }

        get(absPath) match {
            case Some(existing) => {
                val oldCompileTime = existing.compileTime;
                val newCompileTime = fresh.compileTime;

                if newCompileTime >= oldCompileTime // why would they be equal?
                then Replace(before=existing, after=push());
                else NoChange(existing);
            }
            case None => Add(fresh=push());
        }
    }

    def get(refType: ReferenceType) : Option[CfSourceFileWrapper] = refSet.get(refType);
    def get(absPath: String) : Option[CfSourceFileWrapper] = pathToRef.get(absPath);

    def remove(refType: ReferenceType) : Unit = {
        get(refType) match {
            case Some(cfSourceFileRef) => {
                refSet.remove(refType);
                pathToRef.remove(cfSourceFileRef.absPath);
            }
            case None => ()
        }
    }
    class Foo {}
}

class BreakpointManager(
    newVmBreakpoint: (loc: Location) => jdi_BreakpointRequest,
    deleteVmBreakpoints: (breakpointRequests: IterableOnce[jdi_BreakpointRequest]) => Unit) {

    // map from class file references to their respective jdi breakpoint event requests
    private val breakpointJdiRequests = mutable.Map[ReferenceType, immutable.Set[jdi_BreakpointRequest]]();
    private val breakpointClientRequests = mutable.Map[String, List[lsp4j_SourceBreakpoint]]();

    def move(from: CfSourceFileWrapper, to: CfSourceFileWrapper) : Unit = {
        breakpointJdiRequests.get(from.refType) match {
            case Some(breakpointClientRequests) => {
                deleteVmBreakpoints(breakpointClientRequests);
                breakpointJdiRequests.remove(from.refType);
            }
            case None => {}
        }
        tryPushClientBreakpointRequestsToJvm(to);
    }

    private def getLineMapping(refType: ReferenceType) : Map[Int, Location] = {
        @tailrec def worker(remainingLocations: Seq[Location], lineMapping: Map[Int, Location]) : Map[Int, Location] = {
            remainingLocations match {
                case loc +: rest => worker(rest, lineMapping + (loc.lineNumber -> loc))
                case Nil => lineMapping
            }
        }
        val allLines = refType.allLineLocations();
        worker(allLines.asScala.toList, Map[Int, Location]())
    }

    def register(absPath: String, breakpoints: List[lsp4j_SourceBreakpoint]) = {
        breakpointClientRequests.put(absPath, breakpoints);
    }

    def tryPushClientBreakpointRequestsToJvm(cfSourceFileWrapper: CfSourceFileWrapper) = {
        breakpointClientRequests.get(cfSourceFileWrapper.absPath) match {
            case Some(breakpoints) => put(cfSourceFileWrapper, breakpoints);
            case None => {}
        }
    }

    // "put" as in the HTTP idempotency semantics -- the client should send all breakpoints for some reftype, not just a delta
    def put(cfSourceFileWrapper: CfSourceFileWrapper, breakpoints: List[lsp4j_SourceBreakpoint]) : Unit = {
        val refType = cfSourceFileWrapper.refType;
        val refTypeLineMapping = getLineMapping(refType);

        def findExistingRequestByLineNumber(jdiBreakpointRequests: Set[jdi_BreakpointRequest], targetLineNumber: Int) : Option[jdi_BreakpointRequest] = {
            jdiBreakpointRequests.find((bp) => bp.location.lineNumber == targetLineNumber);
        }

        @tailrec def worker(
            existing: immutable.Set[jdi_BreakpointRequest],
            fresh: List[lsp4j_SourceBreakpoint],
            persistable: immutable.Set[jdi_BreakpointRequest]) : immutable.Set[jdi_BreakpointRequest] = {
            fresh match {
                case newRequest :: rest => {
                    findExistingRequestByLineNumber(existing, newRequest.getLine()) match {
                        case Some(jdiBreakpointRequest) => {
                            worker(existing - jdiBreakpointRequest, rest, persistable + jdiBreakpointRequest)
                        }
                        case None => {
                            val loc = refTypeLineMapping.get(newRequest.getLine());
                            if (loc.isDefined)
                            then {
                                val newRequest = newVmBreakpoint(loc.get)
                                newRequest.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD);
                                newRequest.enable()
                                worker(existing, rest, persistable + newRequest)
                            }
                            else {
                                // fixme: should tell client we couldn't bind this breakpoint to a location
                                // for now, we pretend the client didn't ask for it
                                worker(existing, rest, persistable)
                            }
                        }
                    }
                }
                case Nil => {
                    // we've dealt with all requested breakpoints
                    // any left in `existing` need to be removed from the target VM
                    deleteVmBreakpoints(existing.seq)
                    persistable
                }
            }
        }    

        val existing = breakpointJdiRequests.get(refType) match {
            case Some(existing) => existing;
            case None => immutable.Set[jdi_BreakpointRequest]();
        }

        breakpointJdiRequests.put(
            refType,
            worker(existing, breakpoints, Set[jdi_BreakpointRequest]()));
    }
}

class ThreadManager {
    val pageContextMap = mutable.Map[ThreadReference, PageContext]();
    val threadMap = mutable.Map[Int, ThreadReference]();
    val paused = mutable.Set[Int](); // if a thread id is not in here, it is running


    def add(jvmThreadRef: ThreadReference) : Unit = {
        threadMap.put(jvmThreadRef.hashCode(), jvmThreadRef);
    }

    def addPageContext(threadRef: ThreadReference, pageContext: PageContext) : Unit = pageContextMap.put(threadRef, pageContext);

    def get(threadRefHash: Int) : Option[ThreadReference] = threadMap.get(threadRefHash);
    def getPageContext(threadRef: ThreadReference) : Option[PageContext] = pageContextMap.get(threadRef);

    def remove(threadRefHash: Int) : Unit = {
        threadMap.remove(threadRefHash);
    }

    def remove(jvmThreadRef: ThreadReference) : Unit = {
        remove(jvmThreadRef.hashCode());
    }

    def markPaused(threadRef: ThreadReference) : Unit = paused.add(threadRef.hashCode());
    def markPaused(threadRefs: Iterable[ThreadReference]) : Unit = threadRefs.foreach((v) => markPaused(v));

    def markRunning(threadRef: ThreadReference) : Unit = paused.remove(threadRef.hashCode());
    def markRunning(threadRefs: Iterable[ThreadReference]) : Unit = threadRefs.foreach((v) => markRunning(v));

    def getPausedListing() : Iterable[ThreadReference] = {
        val result = ArrayBuffer[ThreadReference]();
        for (threadId <- paused) {
            threadMap.get(threadId) match {
                case Some(threadRef) => result += threadRef;
                case None => ()
            }
        }
        result;
    }

    def getThreadListing() : Iterable[ThreadReference] = threadMap.values;

    def resumeAll() : Unit = {
        getPausedListing().foreach((threadRef) => {
            threadRef.resume();
            markRunning(threadRef);
        });
    }
}

class CfVirtualMachine(vm: VirtualMachine, client: IDebugProtocolClient, stderr: Option[OutputStream] = None) {
    private val refTypes = mutable.Map[String, ReferenceType]();

    private val sourceManager = SourceManager();
    private var threadManager = ThreadManager();
    private val breakpointManager = BreakpointManager(
        newVmBreakpoint = vm.eventRequestManager().createBreakpointRequest,
        deleteVmBreakpoints = (bps: IterableOnce[jdi_BreakpointRequest]) => {
            val vs = ArrayList[jdi_BreakpointRequest]();
            for (bp <- bps.iterator) { vs.add(bp) };
            vm.eventRequestManager().deleteEventRequests(vs);
        }
    );

    private val classPrepareRequest : ClassPrepareRequest = {
        val request = vm.eventRequestManager().createClassPrepareRequest();
        // suspend to move breakpoints from old to new file, and then restart the thread prior to invoking the page 
        // we should filter this event request to just CF class files;
        // we effectively do that in the event pump handler
        request.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD);
        request;
    }
    private val classUnloadRequest : ClassUnloadRequest = {
        val request = vm.eventRequestManager().createClassUnloadRequest();
        request.setSuspendPolicy(EventRequest.SUSPEND_NONE);
        request;
    }
    private val threadStartRequest : ThreadStartRequest = {
        val request = vm.eventRequestManager().createThreadStartRequest();
        request.setSuspendPolicy(EventRequest.SUSPEND_NONE);
        request;
    }
    private val threadDeathRequest : ThreadDeathRequest = {
        val request = vm.eventRequestManager().createThreadDeathRequest();
        request.setSuspendPolicy(EventRequest.SUSPEND_NONE);
        request;
    }
    
    def dispose() : Unit = {
        vm.dispose()
    }

    // we can assume that `classUtil` is non-null; if we are unable to make it non-null during initialization, we can't do anything else
    private var classUtil : ClassUtilMirror = null;
    var mirrors : JvmMirrors = null;
    def guardLoadedMirrors(threadRef: ThreadReference) : Unit = {
        if (mirrors == null) {
            mirrors = JvmMirrors(threadRef, classUtil);
        }
    }

    private def kickoff() = {
        def indexVmRefType(refType: ReferenceType) : Option[ReferenceType] = {
            def tryMatchCfCompiledClassFile(refType: ReferenceType) : Boolean = {
                CfVirtualMachine.maybeCfSourceFilePath(refType) match {                    
                    case None => false;
                    case Some(sourcePath) => {
                        val absPath = sourcePath;

                        if (Files.exists(File(absPath).toPath())) {
                            sourceManager.addOrGet(refType, absPath.toString());
                        }
                        else {
                            // no-op -- file doesn't exist, probably we appended projectRoot onto an already absolute path?
                            // when does the engine compile a class file with an absolute sourcename and when is it relative to a project?
                        }
                        
                        true;
                    }
                }
            }

            def matchJavaClassFile(refType: ReferenceType) : Boolean = {
                // all we need is ClassUtil, we have to wait to load all other classes through ClassUtil when we get a thread on which we can invoke its static methods
                refType.name() match {
                    case ClassUtilMirror.typename => {
                        val loadClass = refType.methodsByName("loadClass", "(Ljava/lang/String;)Ljava/lang/Class;");
                        classUtil = ClassUtilMirror(classType = refType.asInstanceOf[ClassType], loadClass = loadClass.get(0));
                        true;
                    }
                    case _ => false;
                }
            }

            @tailrec
            def indexVmRefType(refType: ReferenceType, fs: ((ReferenceType) => Boolean)*) : Option[ReferenceType] = {
                fs match {
                    case f +: rest => if f(refType) then None else indexVmRefType(refType, rest*);
                    case Nil => Some(refType);
                }
            }

            indexVmRefType(refType, tryMatchCfCompiledClassFile, matchJavaClassFile);
        }

        vm.allClasses().asScala.foreach(indexVmRefType);
        vm.allThreads().asScala.foreach((threadRef) => threadManager.add(threadRef));

        classPrepareRequest.enable();
        classUnloadRequest.enable();
        threadStartRequest.enable();

        threadDeathRequest.enable();
    }

    def putBreakpoints(source: String, breakpoints: List[lsp4j_SourceBreakpoint]) : Unit = {
        breakpointManager.register(source, breakpoints);

        sourceManager.get(source) match {
            case Some(cfSourceFileRef) => {
                breakpointManager.tryPushClientBreakpointRequestsToJvm(cfSourceFileRef);
            }
            case None => ()
        }
    }

    //var fixme_currentMethodExitRequest : MethodExitRequest = null;
    var fixme_currentStepRequest : StepRequest = null;

    def step(threadId: Int) : Unit = {
        synchronized {
            threadManager.get(threadId) match {
                case Some(threadRef) => {
                    val allFrames = threadRef.frames();
                    fixme_currentStepRequest = vm.eventRequestManager().createStepRequest(threadRef, StepRequest.STEP_LINE, StepRequest.STEP_OVER);
                    
                    fixme_currentStepRequest.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD);
                    
                    fixme_currentStepRequest.addClassFilter(mirrors.pageBase.referenceType);
                    
                    fixme_currentStepRequest.enable();

                    // fixme_currentMethodExitRequest = vm.eventRequestManager().createMethodExitRequest();
                    // fixme_currentMethodExitRequest.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD);
                    // fixme_currentMethodExitRequest.enable();

                    threadManager.resumeAll();
                }
                case None => ()
            }
        }
    }

    def stepIn(threadId: Int) : Unit = {
        synchronized {
            threadManager.get(threadId) match {
                case Some(threadRef) => {
                    fixme_currentStepRequest = vm.eventRequestManager().createStepRequest(threadRef, StepRequest.STEP_LINE, StepRequest.STEP_INTO);
                    fixme_currentStepRequest.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD);

                    fixme_currentStepRequest.addClassFilter(mirrors.pageBase.referenceType);

                    fixme_currentStepRequest.enable();

                    // fixme_currentMethodExitRequest = vm.eventRequestManager().createMethodExitRequest();
                    // fixme_currentMethodExitRequest.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD);
                    // fixme_currentMethodExitRequest.enable();

                    threadManager.resumeAll();
                }
                case None => ()
            }
        }
    }

    def stepOut(threadId: Int) : Unit = {
        synchronized {
            threadManager.get(threadId) match {
                case Some(threadRef) => {
                    fixme_currentStepRequest = vm.eventRequestManager().createStepRequest(threadRef, StepRequest.STEP_LINE, StepRequest.STEP_OUT);
                    fixme_currentStepRequest.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD);

                    fixme_currentStepRequest.addClassFilter(mirrors.pageBase.referenceType);

                    fixme_currentStepRequest.enable();

                    // fixme_currentMethodExitRequest = vm.eventRequestManager().createMethodExitRequest();
                    // fixme_currentMethodExitRequest.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD);
                    // fixme_currentMethodExitRequest.enable();

                    threadManager.resumeAll();
                }
                case None => ()
            }
        }
    }

    def leftIsSubtypeOfRight(suspendedThreadRef: ThreadReference)(l: ClassObjectReference)(r: ClassObjectReference) : Boolean = {
        val boolRef = r.invokeMethod(
            suspendedThreadRef,
            mirrors.klass.isAssignableFrom,
            Array(l).toList.asJava,
            ObjectReference.INVOKE_SINGLE_THREADED).asInstanceOf[BooleanValue];
        return boolRef.value();
    }

    def rightIsSubtypeOfLeft(suspendedThreadRef: ThreadReference)(l: ClassObjectReference)(r: ClassObjectReference) : Boolean = {
        return leftIsSubtypeOfRight(suspendedThreadRef)(r)(l);
    }



    ///////////////////

    def getStackTrace(args: StackTraceArguments) : StackTraceResponse = {
        val v = StackTraceResponse();
        // fixme_alwaysOneFrame <-- kludge: has been set in BreakpointEvent handler
        v.setStackFrames(Array(fixme_alwaysOneFrame));
        v.setTotalFrames(1);
        v;
    }
    
    private class ConsumedFrame(frame: StackFrame, _thread: ThreadReference) {
        // a mis-gen'd frame might throw jdi error 35 -- bad slot, this happens when generating the page's `call` method
        val argumentValues = Try({ frame.getArgumentValues(); }) match {
            case Success(v) => Some(v);
            case Failure(_) => None;
        }
        val thread = _thread;
        val __unsafe__frame = frame;
    }

    private def withConsumedFrame(frame: StackFrame, thread: ThreadReference, f: (consumedFrame: ConsumedFrame) => Unit) = f(ConsumedFrame(frame, thread));

    /**
     * Try to get the PageContext object for a particular jvm frame
     * We try two different methods:
     *   - pull arg0 out of the current method, and check if it is a PageContext (often it is, but this is also buggy w/ regards to an invalid code-gen'd `this` slot in `PageImpl.call`)
     *   - aks ThreadLocalPageContext if it has a PageContext for this thread
     */
    private def maybeGetPageContext(consumedFrame: ConsumedFrame) : Option[PageContext] = {
        val isPageContext = (v: ObjectReference) => rightIsSubtypeOfLeft(consumedFrame.thread)(mirrors.pageContextImpl.classObject)(v.referenceType().classObject());

        val fromJavaMethodArgs = Try({consumedFrame.argumentValues.get.get(0).asInstanceOf[ObjectReference]});

        val pageContextObjectRef = fromJavaMethodArgs match {
            case
                Success(objectRef)
                if isPageContext(objectRef) => Some(objectRef);
            case _ => {
                val fromThreadLocal = Try({
                    mirrors.threadLocalPageContext
                        .referenceType
                        .invokeMethod(
                            consumedFrame.thread,
                            mirrors.threadLocalPageContext.get,
                            ArrayList[Value](),
                            ClassType.INVOKE_SINGLE_THREADED).asInstanceOf[ObjectReference]});
                fromThreadLocal match {
                    case Success(objectRef) if isPageContext(objectRef) => Some(objectRef);
                    case _ => None
                }
            }
        }

        pageContextObjectRef.map((objectRef) => PageContext(this, consumedFrame.thread, objectRef));
    }

    def getThreadListing() : Iterable[ThreadReference] = threadManager.getThreadListing();
    def continue_() : Unit = {
        synchronized {
            // could (should?) use the threadManager, but it needs work on correctly tracking all thread start/stop events 100%
            vm.resume();
        }
    }

    private val fixme_frameToScopeMap = mutable.Map[Int, mutable.Set[CfValueMirror.Scope]]();
    private val fixme_alwaysOneFrame = lsp4j_StackFrame(); // we set this on every break/step event, we need to figure out how to get at all the cf frames, and handle threads?
    private var fixme_currentPageContext : PageContext = null;

    // unsafe -- we assume for dev reasons that frameId always returns a scope
    def getScopeMirrorsForFrame(frameId: Int) : mutable.Set[CfValueMirror.Scope] = fixme_frameToScopeMap.get(frameId).get;

    def getCurrentVarScope() = fixme_currentPageContext.variablesScope();

    @tailrec
    private def eventPump() : Unit = {
        val eventSet = vm.eventQueue().remove(); // blocks
        synchronized { // isn't this synchronized by virtue of the blocking call above?
            try {
                eventSet.asScala.foreach(event => {
                    event match {
                        case event : ClassPrepareEvent => {
                            CfVirtualMachine.maybeCfSourceFilePath(event.referenceType) match {
                                case Some(absPath) => {
                                    sourceManager.addOrGet(event.referenceType, absPath) match {
                                        case SourceManager.AddOrGetResult.Replace(before, after) => {
                                            // move existing breakpoints from old classfile to new classfile
                                            breakpointManager.move(before, after);
                                            // also delete the old reftype from the source manager?
                                        }
                                        case _ => {}
                                    }
                                }
                                case None => ();
                            }

                            event.thread().resume();
                        }
                        case event : ClassUnloadEvent => {
                        }
                        case event : ThreadStartEvent => {
                            threadManager.add(event.thread())
                        }
                        case event: ThreadDeathEvent => {
                            threadManager.remove(event.thread())
                        }
                        // case event : MethodExitEvent
                        //     if fixme_currentMethodExitRequest != null => {
                        //     vm.eventRequestManager().deleteEventRequest(event.request());
                        //     fixme_currentMethodExitRequest = null;

                        // }
                        case event : StepEvent => {
                            vm.eventRequestManager().deleteEventRequest(event.request());
                            withConsumedFrame(event.thread().frame(0), event.thread, (consumedFrame) => {
                                threadManager.markPaused(consumedFrame.thread);

                                if // fixme -- add to class filters for step request
                                    event.location().declaringType() == mirrors.pageBase.referenceType
                                    || event.location().declaringType() == mirrors.componentPageImpl.referenceType
                                    || Try({event.location().declaringType().asInstanceOf[ClassType].sourceName() == "/lucee/Component.cfc"}).getOrElse(false) // might need "ends in" if we're doing abspaths
                                then {
                                    // no-op, keep stepping, we are resolving a page for a cfinclude
                                    stepIn(consumedFrame.thread.hashCode());
                                    ///////////////
                                    // fixme_currentStepRequest = vm.eventRequestManager().createStepRequest(threadRef, StepRequest.STEP_MIN, StepRequest.STEP_INTO);
                                    // fixme_currentStepRequest.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD);
                                    // fixme_currentStepRequest.addClassFilter(pageBaseMirror.refType);
                                    // fixme_currentStepRequest.enable();
                                    // threadRef.resume();
                                    //////////////
                                }
                                else maybeGetPageContext(consumedFrame) match {
                                    case Some(pageContext) => {
                                        val refType = event.location().declaringType();
                                        fixme_currentPageContext = pageContext;
                
                                        val source = {
                                            val sourceWrapper = sourceManager.addOrGet(refType, refType.sourceName()) match {
                                                case SourceManager.AddOrGetResult.Replace(existing, fresh) => {
                                                    // swap breakpoints here...
                                                    fresh;
                                                }
                                                case otherwise => otherwise.newest;
                                            }
                                            
                                            val file = java.io.File(sourceWrapper.absPath);
                                            val source = lsp4j_Source();
                                            source.setName(file.getName());
                                            source.setPath(file.getPath());

                                            source;
                                        }
                
                                        fixme_alwaysOneFrame.setId(1)
                                        fixme_alwaysOneFrame.setName("stacktop");
                                        fixme_alwaysOneFrame.setLine(event.location().lineNumber());
                                        fixme_alwaysOneFrame.setSource(source);

                                        fixme_frameToScopeMap.put(fixme_alwaysOneFrame.getId(), mutable.Set[CfValueMirror.Scope]());

                                        def maybePushScope(scopeStruct: Option[CfValueMirror.Struct], scopeName: ScopeName) = {
                                            scopeStruct match {
                                                case Some(scopeStruct) => {
                                                    fixme_frameToScopeMap
                                                        .get(fixme_alwaysOneFrame.getId())
                                                        .get // unsafed unchecked Option.get
                                                        .add(CfValueMirror.wrapStructAsScope(scopeStruct, scopeName));
                                                }
                                                case None => ()
                                            }
                                        }

                                        maybePushScope(pageContext.variablesScope(), "variables");
                                        maybePushScope(pageContext.localScope(), "local");
                                        maybePushScope(pageContext.argumentsScope(), "arguments");

                                        threadManager.markPaused(consumedFrame.thread);
                
                                        val stoppedEvent = StoppedEventArguments();
                                        stoppedEvent.setReason(StoppedEventArgumentsReason.STEP);
                                        stoppedEvent.setThreadId(consumedFrame.thread.hashCode());
                                        client.stopped(stoppedEvent)
                                    }
                                    case None => {
                                        // couldn't find a PageContext, there's nothing interesting we can do without it
                                        // resume, and don't notify the client that we hit this breakpoint
                                        // event.thread().resume();
                                        threadManager.resumeAll();
                                    }
                                }
                            });
                        }
                        case event : BreakpointEvent => {
                            withConsumedFrame(event.thread().frame(0), event.thread, (consumedFrame) => {
                                guardLoadedMirrors(consumedFrame.thread);

                                if (fixme_currentStepRequest != null) {
                                    fixme_currentStepRequest.disable();
                                    fixme_currentStepRequest = null;
                                }

                                maybeGetPageContext(consumedFrame) match {
                                    case Some(pageContext) => {
                                        val refType = event.location().declaringType();
                                        fixme_currentPageContext = pageContext;
                
                                        val source = {
                                            val source = lsp4j_Source();
                                            sourceManager.get(refType) match {
                                                case Some(cfSourceFileWrapper) => {
                                                    val file = java.io.File(cfSourceFileWrapper.absPath);
                                                    source.setName(file.getName());
                                                    source.setPath(file.getPath());
                                                }
                                                case None => {
                                                    source.setName("<<unknown>>")
                                                    source.setPath("");
                                                }
                                            }
                
                                            source;
                                        }
                
                                        fixme_alwaysOneFrame.setId(1)
                                        fixme_alwaysOneFrame.setName("stacktop");
                                        fixme_alwaysOneFrame.setLine(event.location().lineNumber());
                                        fixme_alwaysOneFrame.setSource(source);

                                        fixme_frameToScopeMap.put(fixme_alwaysOneFrame.getId(), mutable.Set[CfValueMirror.Scope]());

                                        def maybePushScope(scopeStruct: Option[CfValueMirror.Struct], scopeName: ScopeName) = {
                                            scopeStruct match {
                                                case Some(scopeStruct) => {
                                                    fixme_frameToScopeMap
                                                        .get(fixme_alwaysOneFrame.getId())
                                                        .get // unsafe unchecked option unwrap
                                                        .add(CfValueMirror.wrapStructAsScope(scopeStruct, scopeName));
                                                }
                                                case None => ()
                                            }
                                        }

                                        maybePushScope(pageContext.variablesScope(), "variables");
                                        maybePushScope(pageContext.localScope(), "local");
                                        maybePushScope(pageContext.argumentsScope(), "arguments");

                                        threadManager.markPaused(consumedFrame.thread);
                
                                        val stoppedEvent = StoppedEventArguments();
                                        stoppedEvent.setReason(StoppedEventArgumentsReason.BREAKPOINT);
                                        stoppedEvent.setThreadId(consumedFrame.thread.hashCode());
                                        client.stopped(stoppedEvent)
                                    }
                                    case None => {
                                        // couldn't find a PageContext, there's nothing interesting we can do without it
                                        // resume, and don't notify the client that we hit this breakpoint
                                        event.thread().resume();
                                    }
                                }
                            });
                        }
                        // case event : LocatableEvent => {
                        //     val threadRef = event.thread();
                        //     val stackFrame = threadRef.frame(0);
                        //     val pageContext = getPageContext(stackFrame);
                        //     pageContext.variablesScope().keyArray().foreach((key) => {
                        //         println("KEY: " + key);
                        //     })
                        // }
                        case _ => ()
                    }
                })
            }
            catch {
                case any => {
                    //any.getStackTrace().foreach((v) => debugOut(v.toString()));
                }
            }
        }

        eventPump();
    }

    kickoff();
    Thread(() => eventPump()).start();
}

object CfVirtualMachine {
    // foo(refType, projectRoot) => Option[CfSourceFile]
    // probably smarter way to identify cf class files is to see if they extend Page or PageImpl or whatever the appropriate parent class is
    // for now we just look to see if the source path ends in cfm|cfml|cfc
    private val cfmOrCfcPattern = """(?i)\.(cfm|cfml|cfc)$""".r;
    private val DEFAULT_STRATUM : Null = null;

    type SourcePath = String;

    def maybeCfSourceFilePath(refType: ReferenceType) : Option[SourcePath] = {
        Try(refType.sourceName()) match {
            case Success(sourceName) =>
                if cfmOrCfcPattern.findFirstIn(sourceName).isDefined
                then Some(sourceName)
                else None
            case Failure(_) => None
        }
    }
}

class CfStruct(val cfvm: CfVirtualMachine, var threadRef: ThreadReference, val objectRef: ObjectReference) extends ObjectReference {
    export objectRef.{hashCode => _, equals => _, *};

    def keyArray() : Iterable[StringReference] = {
        val keySetRef : ObjectReference = invokeMethod(threadRef, cfvm.mirrors.map.keySet, ArrayList[Value](), ObjectReference.INVOKE_SINGLE_THREADED).asInstanceOf[ObjectReference];
        val arrayRef : ArrayReference = keySetRef.invokeMethod(threadRef, cfvm.mirrors.set.toArray, ArrayList[Value](), ObjectReference.INVOKE_SINGLE_THREADED).asInstanceOf[ArrayReference];
        arrayRef.getValues().asScala.map(_.asInstanceOf[StringReference]);
    }

    def get(key: StringReference) : CfValueMirror = {
        val argList = {
            val argList = ArrayList[Value]();
            argList.add(key);
            argList;
        };
        val result = invokeMethod(threadRef, cfvm.mirrors.map.get, argList, ObjectReference.INVOKE_SINGLE_THREADED);
        CfValueMirror(cfvm)(threadRef, result);
    }

    def count() : Int = {
        return invokeMethod(threadRef, cfvm.mirrors.map.size, ArrayList[Value](), ObjectReference.INVOKE_SINGLE_THREADED).asInstanceOf[IntegerValue].value();
    }

    def foreach(f: (kvPair: (StringReference, CfValueMirror)) => Unit) : Unit = {
        for (key <- keyArray()) {
            f((key, get(key)));
        }
    }
}

/**
 * PageContext is the first arg to most cf-compiled-to-java methods (i.e. <page>.call(PageContext))
 * it stores all the scope information
 */
class PageContext(cfvm: CfVirtualMachine, threadRef: ThreadReference, objectRef: ObjectReference) extends ObjectReference {
    export objectRef.{hashCode => _, equals => _, *};

    def getActiveUDF() : Option[Value] = Option(invokeMethod(threadRef, cfvm.mirrors.pageContextImpl.getActiveUdf, ArrayList[Value](), ObjectReference.INVOKE_SINGLE_THREADED));

    def isInUdf() : Boolean = {
        val activeUdf = getActiveUDF();
        activeUdf match {
            case Some(_) => true
            case None => false
        }
    }

    def variablesScope() : Option[CfValueMirror.Struct] = {
        val result : Value = invokeMethod(
            threadRef,
            cfvm.mirrors.pageContextImpl.variablesScope,
            ArrayList[Value](),
            ObjectReference.INVOKE_SINGLE_THREADED);

        if result == null
        then None
        else Some(CfValueMirror(cfvm)(threadRef, result).asInstanceOf[CfValueMirror.Struct]);
    }

    def argumentsScope() : Option[CfValueMirror.Struct] = {
        val result : Value = invokeMethod(
            threadRef,
            cfvm.mirrors.pageContextImpl.argumentsScope,
            ArrayList[Value](),
            ObjectReference.INVOKE_SINGLE_THREADED);

        if result == null
        then None
        else Some(CfValueMirror(cfvm)(threadRef, result).asInstanceOf[CfValueMirror.Struct]);
    }

    def localScope() : Option[CfValueMirror.Struct] = {
        val result = Try({
            val objectRef = invokeMethod(
                threadRef,
                cfvm.mirrors.pageContextImpl.localScope,
                ArrayList[Value](),
                ObjectReference.INVOKE_SINGLE_THREADED).asInstanceOf[ObjectReference];
            
            if objectRef.referenceType().name == "lucee.runtime.type.scope.LocalNotSupportedScope"
            then None
            else Some(objectRef);
        });

        result match {
            case Success(Some(objectRef)) => Some(CfValueMirror(cfvm)(threadRef, objectRef).asInstanceOf[CfValueMirror.Struct]);
            case _ => None
        }
    }
}

class CfArray(val cfvm: CfVirtualMachine, var threadRef: ThreadReference, val objectRef: ObjectReference) extends ObjectReference {
    export objectRef.{hashCode => _, equals => _, *};

    def len() : Int = {
        return invokeMethod(threadRef, cfvm.mirrors.array.size, ArrayList[Value](), ObjectReference.INVOKE_SINGLE_THREADED).asInstanceOf[IntegerValue].value();
    }

    private def getArrayRef() = invokeMethod(threadRef, cfvm.mirrors.array.toArray, ArrayList[Value](), ObjectReference.INVOKE_SINGLE_THREADED).asInstanceOf[ArrayReference];
    private def getAll() : Iterable[Value] = getArrayRef().getValues().asScala;

    def foreach(f: ((CfValueMirror, Int)) => Unit) : Unit = {
        var i : Int = 0;
        for (e <- getAll()) {
            f(
                (
                    CfValueMirror(cfvm)(threadRef, e),
                    i
                )
            );
            i += 1;
        }
    }
}

type JString = java.lang.String;
type ScopeName = "variables" | "local" | "arguments";
enum CfValueMirror {
    case Scope(underlyingStruct: CfStruct, underlyingStructId: Int, name: ScopeName);
    case String(s: JString);
    case Number(v: Double);
    case Function;
    case ArrowFunction;
    case Struct(cfStruct: CfStruct, id: Int);
    case Array(cfArray: CfArray, id: Int);
    case RawNull;
    // case BoxedNull;
    case Unhandled;
}

object CfValueMirror {
    private val nextId = (() => {
        var id = 1; // start at 1; 0 is used in responses to clients indicating "this is not a complex variable" i.e. it is not structured, just a string or etc.
        () => {
            val v = id;
            id += 1;
            v;
        }
    })();

    private object ComplexVarMap {
        private val objectRefMap = mutable.Map[ObjectReference, CfValueMirror]();
        private val idMap = mutable.Map[Int, CfValueMirror]();

        def getByObjectRef(objRef: ObjectReference) : Option[CfValueMirror] = objectRefMap.get(objRef);
        def getById(id: Int) : Option[CfValueMirror] = idMap.get(id);

        def put(obj: ObjectReference, cfValueMirror: CfValueMirror, id: Int) : Unit = {
            objectRefMap.put(obj, cfValueMirror);
            idMap.put(id, cfValueMirror);
        }
    }

    // unsafe casts, the expectation is that objRefs always represent the same underlying cf value
    private object Factory {
        import CfDebugAdapter.{ CfArray => _CfArray, CfStruct => _CfStruct }

        def CfStruct(cfvm: CfVirtualMachine, threadRef: ThreadReference, objectRef: ObjectReference) : CfValueMirror.Struct = {
            ComplexVarMap.getByObjectRef(objectRef) match {
                case None => {
                    val id = nextId();
                    val v : CfValueMirror.Struct = CfValueMirror.Struct(_CfStruct(cfvm, threadRef, objectRef), id);
                    ComplexVarMap.put(objectRef, v, id);
                    v;
                }
                case Some(existingMirror) => {
                    val cfStructMirror = existingMirror.asInstanceOf[CfValueMirror.Struct];
                    cfStructMirror.cfStruct.threadRef = threadRef;
                    cfStructMirror;
                }
            }
        }

        def CfArray(cfvm: CfVirtualMachine, threadRef: ThreadReference, objectRef: ObjectReference) : CfValueMirror.Array = {
            ComplexVarMap.getByObjectRef(objectRef) match {
                case None => {
                    val id = nextId();
                    val v : CfValueMirror.Array = CfValueMirror.Array(_CfArray(cfvm, threadRef, objectRef), id);
                    ComplexVarMap.put(objectRef, v, id);
                    v;
                }
                case Some(existingMirror) => {
                    val cfArrayMirror = existingMirror.asInstanceOf[CfValueMirror.Array];
                    cfArrayMirror.cfArray.threadRef = threadRef;
                    cfArrayMirror;
                }
            }
        }
    }

    def getValueById(id: Int) = ComplexVarMap.getById(id);

    def wrapStructAsScope(structMirror: CfValueMirror.Struct, scopeName: ScopeName) : Scope = CfValueMirror.Scope(structMirror.cfStruct, structMirror.id, scopeName);

    def apply(cfvm: CfVirtualMachine)(threadRef: ThreadReference, mirroredValue: Value) : CfValueMirror = {
        mirroredValue match {
            case stringRef : StringReference => CfValueMirror.String(stringRef.value());
            case objRef : ObjectReference => {
                val objRefIsSubtypeOf = cfvm.leftIsSubtypeOfRight(threadRef)(objRef.referenceType().classObject());
                if (objRefIsSubtypeOf(cfvm.mirrors.array.classObject)) {
                    Factory.CfArray(cfvm, threadRef, objRef);
                }
                else if (objRefIsSubtypeOf(cfvm.mirrors.map.classObject)) {
                    Factory.CfStruct(cfvm, threadRef, objRef);
                }
                else if (objRefIsSubtypeOf(cfvm.mirrors.double.classObject)) {
                    val asPrimitiveDoubleMirror = objRef.invokeMethod(threadRef, cfvm.mirrors.double.doubleValue, ArrayList[Value](), ObjectReference.INVOKE_SINGLE_THREADED).asInstanceOf[DoubleValue];
                    CfValueMirror.Number(asPrimitiveDoubleMirror.value());
                }
                else if (objRefIsSubtypeOf(cfvm.mirrors.arrowFunction.classObject)) {
                    CfValueMirror.ArrowFunction;
                }
                else if (objRefIsSubtypeOf(cfvm.mirrors.function.classObject)) {
                    CfValueMirror.Function;
                }
                else {
                    CfValueMirror.Unhandled;
                }
            }
            case null => CfValueMirror.RawNull;
            case _ => CfValueMirror.Unhandled;
        }
    }
}
