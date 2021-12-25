package CfDebugAdapter

import scala.jdk.CollectionConverters._;
import scala.annotation.tailrec;
import scala.annotation.targetName;

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

class SourceManager(projectRoot: String) {
    // from the client/IDE, we will get CFM file paths representing a CF file
    private val pathToRef = new java.util.HashMap[String, CfSourceFileWrapper]();
    // from the jvm, we will get `ReferenceType` objects representing a CF file in its compiled-to-classfile form
    private val refSet = new java.util.HashMap[ReferenceType, CfSourceFileWrapper]();

    // we only ever add source files if they are live on the jvm
    // so there is no (string, string) overload
    def add(refType: ReferenceType, absPath: String) : Option[CfSourceFileWrapper] = {
        val fresh = CfSourceFileWrapper(absPath.toString(), refType);

        def push() : Some[CfSourceFileWrapper] = {
            pathToRef.put(fresh.absPath, fresh);
            refSet.put(fresh.refType, fresh);
            Some(fresh);
        }

        get(absPath) match {
            case Some(existing) => {
                val oldCompileTime = existing.compileTime;
                val newCompileTime = fresh.compileTime;

                if newCompileTime >= oldCompileTime // why would they be equal?
                then push();
                else None
            }
            case None => push();
        }
    }

    def get(refType: ReferenceType) : Option[CfSourceFileWrapper] = Option(refSet.get(refType));
    def get(absPath: String) : Option[CfSourceFileWrapper] = Option(pathToRef.get(absPath));

    def remove(refType: ReferenceType) : Unit = {
        get(refType) match {
            case Some(cfSourceFileRef) => {
                refSet.remove(refType);
                pathToRef.remove(cfSourceFileRef.absPath);
            }
            case None => ()
        }
    }
}

class BreakpointManager(
    newVmBreakpoint: (loc: Location) => jdi_BreakpointRequest,
    deleteVmBreakpoints: (breakpointRequests: IterableOnce[jdi_BreakpointRequest]) => Unit) {

    // map from class file references to their respective jdi breakpoint event requests
    private val breakpointJdiRequests = java.util.HashMap[ReferenceType, Set[jdi_BreakpointRequest]]();
    private val breakpointClientRequests = java.util.HashMap[String, List[lsp4j_SourceBreakpoint]]();

    def move(from: CfSourceFileWrapper, to: CfSourceFileWrapper) : Unit = {
        breakpointJdiRequests.get(from.absPath) match {
            case null => ()
            case breakpointClientRequests => {
                deleteVmBreakpoints(breakpointClientRequests);
                breakpointJdiRequests.remove(from.absPath);
            }
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
        debugOut("tryPushClientBreakpointRequestsToJvm, sourceFileWrapper: " + cfSourceFileWrapper + "\n");
        breakpointClientRequests.get(cfSourceFileWrapper.absPath) match {
            case null => ()
            case breakpoints => put(cfSourceFileWrapper, breakpoints);
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
            existing: Set[jdi_BreakpointRequest],
            fresh: List[lsp4j_SourceBreakpoint],
            persistable: Set[jdi_BreakpointRequest]) : Set[jdi_BreakpointRequest] = {
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
            case null => Set[jdi_BreakpointRequest]();
            case existing => existing;
        }

        breakpointJdiRequests.put(
            refType,
            worker(existing, breakpoints, Set[jdi_BreakpointRequest]()));
    }
}

class ThreadManager {
    val pageContextMap = java.util.HashMap[ThreadReference, PageContext]();
    val threadMap = java.util.HashMap[Int, ThreadReference]();
    val paused = java.util.HashSet[Int](); // if a thread id is not in here, it is running


    def add(jvmThreadRef: ThreadReference) : Unit = {
        threadMap.put(jvmThreadRef.hashCode(), jvmThreadRef);
    }

    def addPageContext(threadRef: ThreadReference, pageContext: PageContext) : Unit = pageContextMap.put(threadRef, pageContext);

    def get(threadRefHash: Int) : Option[ThreadReference] = Option(threadMap.get(threadRefHash));
    def getPageContext(threadRef: ThreadReference) : Option[PageContext] = Option(pageContextMap.get(threadRef));

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
        for (threadId <- paused.iterator.asScala) {
            threadMap.get(threadId) match {
                case null => ()
                case threadRef => result += threadRef;
            }
        }
        result;
    }

    def getThreadListing() : Seq[ThreadReference] = threadMap.values().asScala.toList;

    def resumeAll() : Unit = {
        getPausedListing().foreach((threadRef) => {
            threadRef.resume();
            markRunning(threadRef);
        });
    }
}

class CfVirtualMachine(vm: VirtualMachine, client: IDebugProtocolClient, projectRoot: String, stderr: Option[OutputStream] = None) {
    private val refTypes = java.util.HashMap[String, ReferenceType]();

    private val sourceManager = SourceManager(projectRoot);
    private var threadManager = ThreadManager();
    private val breakpointManager = BreakpointManager(
        newVmBreakpoint = vm.eventRequestManager().createBreakpointRequest,
        deleteVmBreakpoints = (bps: IterableOnce[jdi_BreakpointRequest]) => {
            val vs = ArrayList[jdi_BreakpointRequest]();
            for (bp <- bps.iterator) { vs.add(bp) };
            debugOut("deleting breakpoints:");
            vs.asScala.foreach((bp) => debugOut(bp.toString()));
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

    def debugOut(msg: String) : Unit = {
        stderr match {
            case Some(outputStream) => {
                outputStream.write(utf8(msg + "\n"));
            }
            case None => ()
        }
    }

    private def kickoff() = {
        def indexVmRefType(refType: ReferenceType) : Option[ReferenceType] = {
            def tryMatchCfCompiledClassFile(refType: ReferenceType) : Boolean = {
                CfVirtualMachine.maybeCfSourceFilePath(refType) match {                    
                    case None => false;
                    case Some(sourcePath) => {
                        val absPath = classFileSourceFileAttributeToAbsPath(sourcePath);

                        if (Files.exists(absPath)) {
                            sourceManager.add(refType, absPath.toString());
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
                refType.name() match {
                    case
                        // misc java utils
                        MapMirror.typename
                        | SetMirror.typename
                        | PageImplMirror.typename
                        | PageContextImplMirror.typename
                        | CallStackGetMirror.typename
                        | ClassMirror.typename
                        // cf runtime types
                        | ThreadLocalPageContextMirror.typename
                        | ComponentMirror.typename
                        | StructMirror.typename
                        | ArrayMirror.typename
                        | FunctionMirror.typename
                        | ArrowFunctionMirror.typename
                        | DoubleMirror.typename => {
                        refTypes.put(refType.name(), refType);
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

        // val mer = vm.eventRequestManager().createMethodEntryRequest();
        // mer.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD);

    }

    /**
     * compiled files seem to get a source attribute "rooted" to a web folder, like "/index.cfm"
     * that looks like an abs path, but it's not; we want to drop the root indicator and then join
     * the actual project root to it
     * 
     * this might be weird on windows
     */
    private def classFileSourceFileAttributeToAbsPath(sourceFileAttr: String) : Path = {
        def dropRoot(path: Path) : String = {
            path.getRoot() match {
                case null => path.toString()
                case root => path.toString().substring(root.toString().length)
            }
        }

        Path.of(projectRoot, dropRoot(Path.of(sourceFileAttr)))
    }

    private def dumpBreakpoints(s: String = "") = {
        debugOut("\n\n" + s + " >> BREAKPOINTS...\n\n");
        vm.eventRequestManager().breakpointRequests().asScala.foreach((bpr) => {
            debugOut(bpr.toString() + " @ " + bpr.location().declaringType() + "\n");
        })
    }

    def putBreakpoints(source: String, breakpoints: List[lsp4j_SourceBreakpoint]) : Unit = {
        breakpointManager.register(source, breakpoints);

        sourceManager.get(source) match {
            case Some(cfSourceFileRef) => {
                breakpointManager.tryPushClientBreakpointRequestsToJvm(cfSourceFileRef);
            }
            case None => ()
        }

        dumpBreakpoints("putBreakpoints");
    }

    //var fixme_currentMethodExitRequest : MethodExitRequest = null;
    var fixme_currentStepRequest : StepRequest = null;

    def step(threadId: Int) : Unit = {
        synchronized {
            threadManager.get(threadId) match {
                case Some(threadRef) => {
                    fixme_currentStepRequest = vm.eventRequestManager().createStepRequest(threadRef, StepRequest.STEP_LINE, StepRequest.STEP_OVER);
                    fixme_currentStepRequest.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD);
                    fixme_currentStepRequest.addCountFilter(1);
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

    lazy val mapMirror : MapMirror = {
        refTypes.get(MapMirror.typename) match {
            case null => throw Exception();
            case refType => {
                MapMirror(
                    refType=refType,
                    keySet = refType.methodsByName("keySet").get(0),
                    get = refType.methodsByName("get").get(0),
                    size = refType.methodsByName("size").get(0)
                )
            }
        }
    }

    lazy val setMirror : SetMirror = {
        refTypes.get(SetMirror.typename) match {
            case null => throw Exception("failure while building Set mirror");
            case refType => {
                val toArray = refType.methodsByName("toArray", "()[Ljava/lang/Object;");
                SetMirror(refType=refType, toArray = toArray.get(0));
            }
        }
    }

    lazy val pageImplMirror : PageImplMirror = {
        refTypes.get(PageImplMirror.typename) match {
            case null => throw Exception("failure while building PageImpl mirror");
            case refType => {
                PageImplMirror(refType=refType, getCompileTime = refType.methodsByName("getCompileTime").get(0));
            }
        }
    }

    lazy val pageContextImplMirror : PageContextImplMirror = {
        refTypes.get(PageContextImplMirror.typename) match {
            case null => throw Exception("failure while building PageContext mirror");
            case refType => {
                PageContextImplMirror(
                    refType=refType,
                    variablesScope = refType.methodsByName("variablesScope").get(0),
                    argumentsScope = refType.methodsByName("argumentsScope").get(0),
                    localScope = refType.methodsByName("localScope").get(0),
                    getActiveUdf = refType.methodsByName("getActiveUDF").get(0));
            }
        }
    }

    lazy val threadLocalPageContextMirror : ThreadLocalPageContextMirror = {
        refTypes.get(ThreadLocalPageContextMirror.typename) match {
            case null => throw Exception("failure while building ThreadLocalPageContext mirror");
            case refType => {
                val get = refType.methodsByName("get", "()Llucee/runtime/PageContext;")
                ThreadLocalPageContextMirror(refType=refType, get = get.get(0));
            }
        }
    }

    lazy val callStackGetMirror : CallStackGetMirror = {
        refTypes.get(CallStackGetMirror.typename) match {
            case null => throw Exception("failure while building CallStackGet mirror");
            case refType => {
                CallStackGetMirror(refType=refType, call=refType.methodsByName("call").get(0))
            }
        }
    }

    lazy val classMirror : ClassMirror = {
        refTypes.get(ClassMirror.typename) match {
            case null => throw Exception("failure while building Class mirror")
            case refType => {
                ClassMirror(refType=refType, isAssignableFrom = refType.methodsByName("isAssignableFrom").get(0));
            }
        }
    }

    lazy val doubleMirror : DoubleMirror = {
        refTypes.get(DoubleMirror.typename) match {
            case null => throw Exception("failure while building Double mirror")
            case refType => {
                val doubleValue = refType.methodsByName("doubleValue");
                DoubleMirror(refType=refType, doubleValue=doubleValue.get(0));
            }
        }
    }

    def leftIsSubtypeOfRight(suspendedThreadRef: ThreadReference)(l: ClassObjectReference)(r: ClassObjectReference) : Boolean = {
        val boolRef = r.invokeMethod(
            suspendedThreadRef,
            classMirror.isAssignableFrom,
            Array(l).toList.asJava,
            ObjectReference.INVOKE_SINGLE_THREADED).asInstanceOf[BooleanValue];
        return boolRef.value();
    }

    def rightIsSubtypeOfLeft(suspendedThreadRef: ThreadReference)(l: ClassObjectReference)(r: ClassObjectReference) : Boolean = {
        return leftIsSubtypeOfRight(suspendedThreadRef)(r)(l);
    }

    lazy val componentMirror : ComponentMirror = {
        refTypes.get(ComponentMirror.typename) match {
            case null => throw Exception("failure while building Component mirror")
            case refType => {
                ComponentMirror(refType=refType);
            }
        }
    }

    lazy val structMirror : StructMirror = {
        refTypes.get(StructMirror.typename) match {
            case null => throw Exception("failure whlie building Struct mirror");
            case refType => StructMirror(refType=refType);
        }
    }

    lazy val arrayMirror : ArrayMirror = {
        refTypes.get(ArrayMirror.typename) match {
            case null => throw Exception("failure while build Array mirror");
            case refType => {
                val toArray = refType.methodsByName("toArray", "()[Ljava/lang/Object;");
                ArrayMirror(refType=refType, size=refType.methodsByName("size").get(0), toArray=toArray.get(0));
            }
        }
    }

    lazy val functionMirror : FunctionMirror = {
        refTypes.get(FunctionMirror.typename) match {
            case null => throw Exception("failure while building Function mirror");
            case refType => FunctionMirror(refType = refType);
        }
    }

    lazy val arrowFunctionMirror : ArrowFunctionMirror = {
        refTypes.get(ArrowFunctionMirror.typename) match {
            case null => throw Exception("failure while building ArrowFunction mirror");
            case refType => ArrowFunctionMirror(refType = refType);
        }
    }
    
    ///////////////////

    def hasSource(path: String) : Boolean = {
        return false;
    }


    def getStackTrace(args: StackTraceArguments) : StackTraceResponse = {
        val v = StackTraceResponse();
        // fixme_alwaysOneFrame <-- kludge: has been set in BreakpointEvent handler
        v.setStackFrames(Array(fixme_alwaysOneFrame));
        v.setTotalFrames(1);
        v;
    }

    private def maybeGetPageContext(frame: StackFrame) : Option[PageContext] = {
        val threadRef = frame.thread(); // get the threadRef now, after invoking anything on the thread the frame is marked as having been restarted and accessing frame throws exceptions
        
        val isPageContext = (v: ObjectReference) => rightIsSubtypeOfLeft(threadRef)(pageContextImplMirror.refType.classObject)(v.referenceType().classObject());

        val fromJavaMethodArgs = Try({ frame.getArgumentValues().get(0).asInstanceOf[ObjectReference]; });

        val pageContextObjectRef = fromJavaMethodArgs match {
            case
                Success(objectRef)
                if isPageContext(objectRef) => Some(objectRef);
            case _ => {
                val fromThreadLocal = Try({
                    threadLocalPageContextMirror
                        .refType
                        .classObject()
                        .invokeMethod(
                            frame.thread(),
                            threadLocalPageContextMirror.get,
                            ArrayList[Value](),
                            ObjectReference.INVOKE_NONVIRTUAL | ObjectReference.INVOKE_SINGLE_THREADED).asInstanceOf[ObjectReference]});
                fromThreadLocal match {
                    case Success(objectRef) if isPageContext(objectRef) => Some(objectRef);
                    case _ => None
                }
            }
        }

        pageContextObjectRef.map((objectRef) => PageContext(this, threadRef, objectRef));
    }

    def getThreadListing() : Seq[ThreadReference] = threadManager.getThreadListing();
    def continue_() : Unit = {
        synchronized {
            threadManager.resumeAll();
        }
    }

    private val fixme_frameToScopeMap = java.util.HashMap[Int, java.util.HashSet[CfValueMirror.Scope]];
    private val fixme_alwaysOneFrame = lsp4j_StackFrame(); // we set this on every break/step event, we need to figure out how to get at all the cf frames, and handle threads?
    private var fixme_currentPageContext : PageContext = null;

    def getScopeMirrorsForFrame(frameId: Int) : java.util.HashSet[CfValueMirror.Scope] = fixme_frameToScopeMap.get(frameId);

    def getCurrentVarScope() = fixme_currentPageContext.variablesScope();

    @tailrec
    private def eventPump() : Unit = {
        val eventSet = vm.eventQueue().remove();
        try {
            eventSet.asScala.foreach(event => {
                event match {
                    case event : ClassPrepareEvent => {
                        debugOut("\t" + event.referenceType.name() + "\n");
                        CfVirtualMachine.maybeCfSourceFilePath(event.referenceType) match {
                            case Some(sourcePath) => {
                                val absPath = classFileSourceFileAttributeToAbsPath(sourcePath).toString();
                                debugOut("\n[class-prepare]:" + event.referenceType().name() + " -- " + absPath + "\n");
                                val maybeExisting = sourceManager.get(absPath);
                                val fresh = sourceManager.add(event.referenceType, absPath);

                                if (fresh == null) debugOut("\nfresh was null\n");

                                debugOut("[class-prepare]:  old compile time: " + maybeExisting.map(_.compileTime.toString()).getOrElse("") + "\n");
                                debugOut("[class-prepare]:  new compile time: " + fresh.map(_.compileTime.toString()).getOrElse("") + "\n");

                                // probably this is a "new compiled version" of an already existing class file
                                // i.e the client made an edit to the sourcefile and refreshed the page
                                (maybeExisting, fresh) match {
                                    case (Some(existing), Some(fresh)) => {
                                        debugOut("[class-prepare]: found existing, attempting to move breakpoints...")
                                        // move existing breakpoints from old classfile to new classfile
                                        breakpointManager.move(existing, fresh);
                                        dumpBreakpoints("[class-prepare]");
                                        // also delete the old reftype from the source manager?
                                    }
                                    case _ => ()
                                }

                                dumpBreakpoints("classPrepare")
                            }
                            case None => ();
                        }

                        event.thread().resume();
                    }
                    case event : ClassUnloadEvent => {
                        debugOut(event.toString());
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
                    // case event : StepEvent => {
                    //     val xxx = Try({
                    //         vm.eventRequestManager().deleteEventRequest(event.request());
                    //         fixme_currentStepRequest = null;
                    //         // if (fixme_currentMethodExitRequest != null) {
                    //         //     vm.eventRequestManager().deleteEventRequest(fixme_currentMethodExitRequest);
                    //         //     fixme_currentMethodExitRequest = null;
                    //         // }
                            
                    //         val threadRef = event.thread();
                    //         val stackFrame = threadRef.frame(0);
                    //         val refType = event.location().declaringType();

                    //         if (refType == pageContextImplMirror.refType) {
                    //             // no-op, we've stepped out of <page>.call() and back into <PageContext>
                    //             // meaning we're done with the CF file and the jvm is back in CF engine code
                    //             threadRef.resume();
                    //             threadManager.resumeAll();
                    //         }
                    //         else {
                    //             val pageContext = getPageContext(stackFrame);
                    //             fixme_currentPageContext = pageContext;

                    //             val source = {
                    //                 val source = lsp4j_Source();
                    //                 sourceManager.get(refType) match {
                    //                     case Some(cfSourceFileWrapper) => {
                    //                         val file = java.io.File(cfSourceFileWrapper.absPath);
                    //                         source.setName(file.getName());
                    //                         source.setPath(file.getPath());
                    //                     }
                    //                     case None => {
                    //                         source.setName("<<unknown>>")
                    //                         source.setPath("");
                    //                     }
                    //                 }

                    //                 source;
                    //             }

                    //             fixme_alwaysOneFrame.setId(1)
                    //             fixme_alwaysOneFrame.setName("stacktop");
                    //             fixme_alwaysOneFrame.setLine(event.location().lineNumber());
                    //             fixme_alwaysOneFrame.setSource(source);

                    //             threadManager.markPaused(threadRef);

                    //             val stoppedEvent = StoppedEventArguments();
                    //             stoppedEvent.setReason(StoppedEventArgumentsReason.STEP);
                    //             stoppedEvent.setThreadId(threadRef.hashCode());
                    //             client.stopped(stoppedEvent);
                    //         }
                    //     })

                    //     if (xxx.isFailure) { debugOut(xxx.failed.get.toString()) }
                    // }
                    case event : BreakpointEvent => {
                        val threadRef = event.thread();
                        val stackFrame = threadRef.frame(0);
                        maybeGetPageContext(stackFrame) match {
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

                                fixme_frameToScopeMap.put(fixme_alwaysOneFrame.getId(), java.util.HashSet[CfValueMirror.Scope]());

                                def maybePushScope(scopeStruct: Option[CfValueMirror.Struct], scopeName: ScopeName) = {
                                    scopeStruct match {
                                        case Some(scopeStruct) => {
                                            fixme_frameToScopeMap.get(fixme_alwaysOneFrame.getId())
                                            .add(CfValueMirror.wrapStructAsScope(scopeStruct, scopeName));
                                        }
                                        case None => ()
                                    }
                                }

                                maybePushScope(pageContext.variablesScope(), "variables");
                                maybePushScope(pageContext.localScope(), "local");
                                maybePushScope(pageContext.argumentsScope(), "arguments");

                                threadManager.markPaused(threadRef);
        
                                val stoppedEvent = StoppedEventArguments();
                                stoppedEvent.setReason(StoppedEventArgumentsReason.BREAKPOINT);
                                stoppedEvent.setThreadId(threadRef.hashCode());
                                client.stopped(stoppedEvent)
                            }
                            case None => {
                                // couldn't find a PageContext, there's nothing interesting we can do without it
                                // resume, and don't notify the client that we hit this breakpoint
                                event.thread().resume();
                            }
                        }

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
                val stacktop = if any.getStackTrace().length > 0
                    then any.getStackTrace()(0).toString()
                    else "<<nil>>";
                debugOut(any.toString() + ":\n" + stacktop);
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
        val keySetRef : ObjectReference = invokeMethod(threadRef, cfvm.mapMirror.keySet, ArrayList[Value](), ObjectReference.INVOKE_SINGLE_THREADED).asInstanceOf[ObjectReference];
        val arrayRef : ArrayReference = keySetRef.invokeMethod(threadRef, cfvm.setMirror.toArray, ArrayList[Value](), ObjectReference.INVOKE_SINGLE_THREADED).asInstanceOf[ArrayReference];
        arrayRef.getValues().asScala.map(_.asInstanceOf[StringReference]);
    }

    def get(key: StringReference) : CfValueMirror = {
        val argList = {
            val argList = ArrayList[Value]();
            argList.add(key);
            argList;
        };
        val result = invokeMethod(threadRef, cfvm.mapMirror.get, argList, ObjectReference.INVOKE_SINGLE_THREADED);
        CfValueMirror(cfvm)(threadRef, result);
    }

    def count() : Int = {
        return invokeMethod(threadRef, cfvm.mapMirror.size, ArrayList[Value](), ObjectReference.INVOKE_SINGLE_THREADED).asInstanceOf[IntegerValue].value();
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

    def getActiveUDF() : Option[Value] = Option(invokeMethod(threadRef, cfvm.pageContextImplMirror.getActiveUdf, ArrayList[Value](), ObjectReference.INVOKE_SINGLE_THREADED));

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
            cfvm.pageContextImplMirror.variablesScope,
            ArrayList[Value](),
            ObjectReference.INVOKE_SINGLE_THREADED);

        if result == null
        then None
        else Some(CfValueMirror(cfvm)(threadRef, result).asInstanceOf[CfValueMirror.Struct]);
    }

    def argumentsScope() : Option[CfValueMirror.Struct] = {
        val result : Value = invokeMethod(
            threadRef,
            cfvm.pageContextImplMirror.argumentsScope,
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
                cfvm.pageContextImplMirror.localScope,
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
        return invokeMethod(threadRef, cfvm.arrayMirror.size, ArrayList[Value](), ObjectReference.INVOKE_SINGLE_THREADED).asInstanceOf[IntegerValue].value();
    }

    private def getArrayRef() = invokeMethod(threadRef, cfvm.arrayMirror.toArray, ArrayList[Value](), ObjectReference.INVOKE_SINGLE_THREADED).asInstanceOf[ArrayReference];
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
        private val objectRefMap = java.util.HashMap[ObjectReference, CfValueMirror]();
        private val idMap = java.util.HashMap[Int, CfValueMirror]();

        def getByObjectRef(objRef: ObjectReference) : Option[CfValueMirror] = {
            objectRefMap.get(objRef) match {
                case null => None
                case existingMirror => Some(existingMirror);
            }            
        }

        def getById(id: Int) : Option[CfValueMirror] = {
            idMap.get(id) match {
                case null => None
                case existingMirror => Some(existingMirror);
            }
        }

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
                if (objRefIsSubtypeOf(cfvm.arrayMirror.refType.classObject())) {
                    Factory.CfArray(cfvm, threadRef, objRef);
                }
                else if (objRefIsSubtypeOf(cfvm.mapMirror.refType.classObject())) {
                    Factory.CfStruct(cfvm, threadRef, objRef);
                }
                else if (objRefIsSubtypeOf(cfvm.doubleMirror.refType.classObject())) {
                    val asPrimitiveDoubleMirror = objRef.invokeMethod(threadRef, cfvm.doubleMirror.doubleValue, ArrayList[Value](), ObjectReference.INVOKE_SINGLE_THREADED).asInstanceOf[DoubleValue];
                    CfValueMirror.Number(asPrimitiveDoubleMirror.value());
                }
                else if (objRefIsSubtypeOf(cfvm.arrowFunctionMirror.refType.classObject())) {
                    CfValueMirror.ArrowFunction;
                }
                else if (objRefIsSubtypeOf(cfvm.functionMirror.refType.classObject())) {
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