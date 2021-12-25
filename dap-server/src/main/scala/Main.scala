package CfDebugAdapter

import scala.jdk.CollectionConverters._;
import org.apache.commons.io.output.TeeOutputStream;
import org.apache.commons.io.input.TeeInputStream;

import com.sun.jdi.connect.AttachingConnector;
import com.sun.jdi.Bootstrap;
import com.sun.jdi.VirtualMachine

import java.io.PrintWriter;
import org.eclipse.lsp4j.debug.services.IDebugProtocolClient
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.io.File
import java.nio.file.StandardOpenOption
import scala.util.Try

val debugOutFileStream = Files.newOutputStream(File("/home/anon/cflsd/plugin.rmme.log").toPath(), StandardOpenOption.CREATE, StandardOpenOption.APPEND);
def debugOut(string: String) = debugOutFileStream.write(utf8(string))
val teeOutToDebug = TeeOutputStream(System.out, debugOutFileStream);
def utf8(string: String) : Array[Byte] = string.getBytes(StandardCharsets.UTF_8);

val teeInToDebug = TeeInputStream(System.in, debugOutFileStream);

@main def hello: Unit = 
    val (server, launcher) = CfDebugAdapterProtocolServer(teeInToDebug, teeOutToDebug, false, PrintWriter(System.err));
    server.setCfVmStderr(debugOutFileStream);
    launcher.startListening();

def socketAttachingConnector : AttachingConnector = {
    val vmm = Bootstrap.virtualMachineManager();
    val attachingConnectors = vmm.attachingConnectors();
    val socketAttachingConnector = attachingConnectors.asScala.find(_.name.equals("com.sun.jdi.SocketAttach"));
    return socketAttachingConnector.getOrElse(throw new Exception("Couldn't get a socket attaching connector."));
}

def connect(hostname: String, port: String, connector: AttachingConnector = socketAttachingConnector) : Try[VirtualMachine] = {
    val args = connector.defaultArguments;
    args.get("hostname").setValue(hostname);
    args.get("port").setValue(port);
    return Try(connector.attach(args));
}
