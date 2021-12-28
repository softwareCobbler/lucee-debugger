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

@main def hello: Unit = 
    val (server, launcher) = CfDebugAdapterProtocolServer(System.in, System.out, false, PrintWriter(System.err));
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
