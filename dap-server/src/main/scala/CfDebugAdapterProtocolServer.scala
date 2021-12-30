package CfDebugAdapter

import scala.jdk.CollectionConverters._;

import org.eclipse.lsp4j.debug.Capabilities
import org.eclipse.lsp4j.debug.ConfigurationDoneArguments
import org.eclipse.lsp4j.debug.DisconnectArguments
import org.eclipse.lsp4j.debug.InitializeRequestArguments
import org.eclipse.lsp4j.debug.SetBreakpointsArguments
import org.eclipse.lsp4j.debug.SetBreakpointsResponse
import org.eclipse.lsp4j.debug.Source
import org.eclipse.lsp4j.debug.{Thread, ThreadsResponse}
import org.eclipse.lsp4j.debug.launch.DSPLauncher
import org.eclipse.lsp4j.debug.services.IDebugProtocolClient
import org.eclipse.lsp4j.debug.services.IDebugProtocolServer
import org.eclipse.lsp4j.jsonrpc.Launcher
import org.eclipse.lsp4j.jsonrpc.RemoteEndpoint

import com.sun.jdi.ObjectReference

import java.io.InputStream
import java.io.OutputStream
import java.io.PrintWriter
import java.util.concurrent.CompletableFuture
import org.eclipse.lsp4j.debug.StackTraceArguments
import org.eclipse.lsp4j.debug.StackTraceResponse
import org.eclipse.lsp4j.debug.ScopesArguments
import org.eclipse.lsp4j.debug.ScopesResponse
import org.eclipse.lsp4j.debug.ContinueArguments
import org.eclipse.lsp4j.debug.ContinueResponse
import org.eclipse.lsp4j.debug.Scope
import org.eclipse.lsp4j.debug.VariablesArguments
import org.eclipse.lsp4j.debug.VariablesResponse
import org.eclipse.lsp4j.debug.Variable
import org.eclipse.lsp4j.debug.NextArguments
import org.eclipse.lsp4j.debug.TerminatedEventArguments
import org.eclipse.lsp4j.debug.ContinuedEventArguments
import scala.collection.mutable.ArrayBuffer
import scala.util.Success
import scala.util.Failure
import java.nio.charset.StandardCharsets
import org.eclipse.lsp4j.debug.StepInArguments
import org.eclipse.lsp4j.debug.StepOutArguments
import scala.util.Try

class CfDebugAdapterProtocolServer extends IDebugProtocolServer {
    private var cfvm_ : CfVirtualMachine = null; // assume non-null
    private var clientProxy_ : IDebugProtocolClient = null; // assume non-null
    private var serverLauncher_ : Launcher[IDebugProtocolClient] = null; // assume non-null

    override def initialize(args: InitializeRequestArguments) : CompletableFuture[Capabilities] = {
        val c = Capabilities()
        //c.setSupportsConfigurationDoneRequest(true);
        CompletableFuture.completedFuture(c);
    }
    override def attach(args: java.util.Map[String, Object]) : CompletableFuture[Void] = {
        // ??
        // val log = Logger.getLogger(classOf[RemoteEndpoint].getName());
        // log.log()

        val hostName = Try({args.get("hostName").asInstanceOf[String];});
        val port = Try({args.get("port").asInstanceOf[String];});
        (hostName, port) match {
            case (Success(hostName), Success(port)) => {
                connect(hostName, port, socketAttachingConnector) match {
                    case Success(vm) => {
                        cfvm_ = CfVirtualMachine(vm, clientProxy_);
                        clientProxy_.initialized();
                        return CompletableFuture.completedFuture[Void](null);
                    }
                    case Failure(e) => {
                        serverLauncher_.getRemoteEndpoint().notify("log/error", new Object() {
                            val detail = "Couldn't attach to Lucee server";
                        });
        
                        clientProxy_.terminated(TerminatedEventArguments())
                        return CompletableFuture.completedFuture[Void](null);
                    }
                }
            }
            case _ => {
                serverLauncher_.getRemoteEndpoint().notify("log/error", new Object() {
                    val detail = "Bad hostname or port.";
                });
                clientProxy_.terminated(TerminatedEventArguments());
                return CompletableFuture.completedFuture[Void](null);
            }
        }

    }

    override def continue_(args: ContinueArguments) : CompletableFuture[ContinueResponse] = {
        cfvm_.continue_();
        val result = ContinueResponse();
        return CompletableFuture.completedFuture(result);
    }

    override def configurationDone(args: ConfigurationDoneArguments) : CompletableFuture[Void] = {
        return CompletableFuture.completedFuture[Void](null);
    }

    override def disconnect(args: DisconnectArguments) : CompletableFuture[Void] = {
        if (cfvm_ != null) {
            cfvm_.dispose();
        }
        return CompletableFuture.completedFuture[Void](null);
    }

    private def jdiThreadReferenceToLsp4jThread(threadRef: com.sun.jdi.ThreadReference) : org.eclipse.lsp4j.debug.Thread = {
        val result = org.eclipse.lsp4j.debug.Thread();
        result.setId(threadRef.hashCode());
        result.setName(threadRef.name());
        return result;
    }

    private def scopeMirrorToLsp4jScope(scope: CfValueMirror.Scope) : org.eclipse.lsp4j.debug.Scope = {
        val result = org.eclipse.lsp4j.debug.Scope();
        result.setName(scope.name);
        result.setVariablesReference(scope.underlyingStructId);
        result;
    }

    override def threads() : CompletableFuture[ThreadsResponse] = {
        val response = ThreadsResponse();
        val threads = cfvm_.getThreadListing().toArray.map(jdiThreadReferenceToLsp4jThread);
        response.setThreads(threads);
        return CompletableFuture.completedFuture(response);
    }

    override def setBreakpoints(args: SetBreakpointsArguments) : CompletableFuture[SetBreakpointsResponse] = {
        cfvm_.putBreakpoints(args.getSource().getPath(), args.getBreakpoints().toList);
        return CompletableFuture.completedFuture(SetBreakpointsResponse());
    }

    override def stackTrace(args: StackTraceArguments) : CompletableFuture[StackTraceResponse] = {
        val responseFrames = cfvm_.getStackTrace(args);
        return CompletableFuture.completedFuture(responseFrames);
    }

    override def scopes(args: ScopesArguments) : CompletableFuture[ScopesResponse] = {
        val result = ScopesResponse();
        val scopeMirrors = cfvm_.getScopeMirrorsForFrame(args.getFrameId());
        val scopes = ArrayBuffer[org.eclipse.lsp4j.debug.Scope]();

        for (scopeMirror <- scopeMirrors.asScala) {
            scopes += scopeMirrorToLsp4jScope(scopeMirror);
        }
        
        result.setScopes(scopes.toArray);
        return CompletableFuture.completedFuture(result);
    }

    final val NOT_STRUCTURED : Int = 0;
    private def cfValueMirrorToLsp4jVariableShallow(valueMirror: CfValueMirror) : Variable = {
        valueMirror match {
            case CfValueMirror.Scope(underlyingStruct, id, _) => {
                // shouldn't get here, shallow expansion of scope should occur in response to "scopes" request
                val result = Variable();
                result;
            }
            case CfValueMirror.Struct(cfStruct, id) => {
                val result = Variable();
                val size = cfStruct.count();
                result.setNamedVariables(size);
                result.setVariablesReference(id);
                result.setValue(s"Struct (${size} members)")
                result;
            }
            case CfValueMirror.Array(cfArray, id) => {
                val result = Variable();
                val size = cfArray.len();
                result.setIndexedVariables(size);
                result.setVariablesReference(id);
                result.setValue(s"Array (${size} members)")
                // fixme: check if client sent "supportsVariableType" in initialize args
                // result.setType("Array");

                result;
            }
            case CfValueMirror.String(s) => {
                val v = Variable();
                v.setValue("\"" + s + "\"");
                v.setVariablesReference(NOT_STRUCTURED);
                v;
            }
            case CfValueMirror.Number(d) => {
                val v = Variable();
                v.setValue(d.toString());
                //v.setType("number"); // ?
                v.setVariablesReference(NOT_STRUCTURED);
                v;
            }
            case CfValueMirror.Function => {
                val v = Variable();
                v.setValue("function");
                v.setVariablesReference(NOT_STRUCTURED);
                v;
            }
            case CfValueMirror.ArrowFunction => {
                val v = Variable();
                v.setValue("arrow function");
                v.setVariablesReference(NOT_STRUCTURED);
                v;
            }
            case CfValueMirror.RawNull => {
                val v = Variable();
                v.setValue("null (raw)");
                v.setVariablesReference(NOT_STRUCTURED);
                v;
            }
            case CfValueMirror.Unhandled => {
                val v = Variable();
                v.setValue("unknown");
                v.setVariablesReference(NOT_STRUCTURED);
                v;
            }
        }
    }

    private def cfValueMirrorToLsp4jVariableExpand1(valueMirror: CfValueMirror) : Array[Variable] = {
        valueMirror match {
            case CfValueMirror.Struct(cfStruct, _) => {
                val result = ArrayBuffer[Variable]();
                
                cfStruct.foreach {
                    case (k,v) => {
                        val kvPair = cfValueMirrorToLsp4jVariableShallow(v);
                        kvPair.setName(k.value());
                        result += kvPair;
                    }
                }

                result.toArray;
            }
            case CfValueMirror.Array(cfArray, _) => {
                val result = ArrayBuffer[Variable]();
                
                cfArray.foreach((e,i) => {
                    val v = cfValueMirrorToLsp4jVariableShallow(e);
                    v.setName((i + 1).toString()); // 1 indexing to conform to CF behavior
                    result += v;
                })

                result.toArray;
            }
            case _ => {
                // shouldn't get here, caller should not try to expand a string | number | scope
                Array[Variable]();
            }
        }
    }

    /*

    frame -> scope -> [scope1, scope2...]

    variables[scope1] -> {k: v1, k: v2}
    variables[v1] -> string, trivial
    variables[v2] -> array, for now we just write the whole thing out

    */
    override def variables(args: VariablesArguments) : CompletableFuture[VariablesResponse] = {
        // args.getVariablesReference <-- what var are we looking at
        // for now we are always looking at @!interface variables { ... }
        
        val result = VariablesResponse();
        val members = CfValueMirror.getValueById(args.getVariablesReference) match {
            case Some(cfValueMirror) => cfValueMirrorToLsp4jVariableExpand1(cfValueMirror);
            case None => Array[Variable]()
        };

        result.setVariables(members);
        return CompletableFuture.completedFuture(result);
    }

    override def next(args: NextArguments) : CompletableFuture[Void] = {
        cfvm_.step(args.getThreadId());

        clientProxy_.continued({
            val v = ContinuedEventArguments();
            v.setThreadId(args.getThreadId());
            v.setAllThreadsContinued(true);
            v;
        })

        return CompletableFuture.completedFuture(null);
    }

    override def stepIn(args: StepInArguments) : CompletableFuture[Void] = {
        cfvm_.stepIn(args.getThreadId());

        clientProxy_.continued({
            val v = ContinuedEventArguments();
            v.setThreadId(args.getThreadId());
            v.setAllThreadsContinued(true);
            v;
        })

        return CompletableFuture.completedFuture(null);
    }

    override def stepOut(args: StepOutArguments) : CompletableFuture[Void] = {
        cfvm_.stepOut(args.getThreadId());

        clientProxy_.continued({
            val v = ContinuedEventArguments();
            v.setThreadId(args.getThreadId());
            v.setAllThreadsContinued(true);
            v;
        })

        return CompletableFuture.completedFuture(null);
    }
}

object CfDebugAdapterProtocolServer {
    def apply(in: InputStream, out: OutputStream, validate: Boolean, traceStream: PrintWriter) : (CfDebugAdapterProtocolServer, Launcher[IDebugProtocolClient]) = {
        val server = new CfDebugAdapterProtocolServer();
        val serverLauncher = DSPLauncher.createServerLauncher(server, in, out, validate, traceStream);
        server.clientProxy_ = serverLauncher.getRemoteProxy();
        server.serverLauncher_ = serverLauncher;
        return (server, serverLauncher);
    }
}
