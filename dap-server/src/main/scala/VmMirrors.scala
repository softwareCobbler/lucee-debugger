/**
 * "mirrors" (jdi terminology) representing object types in the target VM
 * the assumption is that there is only a small amount of which we are interested in, so we can "declare" them here
 * the CfVm is responsible for scanning the target VM and making an instance of each of these, if it wants to invoke methods or etc. on them
 */

package CfDebugAdapter;

import com.sun.jdi.ClassType;
import com.sun.jdi.ReferenceType;
import com.sun.jdi.Method;

class MapMirror(
    val refType: ReferenceType,
    val keySet: Method,
    val get: Method,
    val size: Method,
);
object MapMirror {
    val typename = "java.util.Map";
}

class SetMirror(
    val refType: ReferenceType,
    val toArray: Method
);
object SetMirror {
    val typename = "java.util.Set";
}

// compiled page hierarchy:
// TOP -> lucee.runtime.Page -> lucee.runtime.PageImpl -> (actual compiled cf class)
// so this represents a compiled CF source file
class PageImplMirror (
    val refType: ReferenceType,
    val getCompileTime: Method,
)
object PageImplMirror {
    val typename = "lucee.runtime.PageImpl";
}

class ComponentPageImplMirror(
    val refType: ReferenceType,
)
object ComponentPageImplMirror {
    val typename = "lucee.runtime.ComponentPageImpl";
}

class PageBaseMirror(
    val refType: ReferenceType
)
object PageBaseMirror { // abstract base class of all cf-generated page classes
    val typename = "lucee.runtime.Page";
}

// PageContext is where the cf engine places runtime information for things like the page's variables scope, and etc.
// TOP -> javax.servlet.jsp.PageContext -> PageContext -> PageContextImpl
class PageContextImplMirror(
    val refType: ReferenceType,
    val variablesScope: Method,
    val localScope: Method,
    val argumentsScope: Method,
    val getActiveUdf: Method
);
object PageContextImplMirror {
    val typename = "lucee.runtime.PageContextImpl";
}

class CallStackGetMirror(
    val refType: ReferenceType,
    val call: Method
)
object CallStackGetMirror {
    val typename = "lucee.runtime.functions.system.CallStackGet";
}

class ClassMirror(
    val refType: ReferenceType,
    val isAssignableFrom: Method
)
object ClassMirror {
    val typename = "java.lang.Class";
}

//
// cf runtime type mirrors
// strings are raw Strings, and so are mirrored by jdi StringReference
// numbers are raw Doubles, and so are mirrored by ... ?
//

class ThreadLocalPageContextMirror(
    val classType: ClassType,
    val get: Method // static
)
object ThreadLocalPageContextMirror {
    val typename = "lucee.runtime.engine.ThreadLocalPageContext"
}

class ComponentMirror(
    val refType: ReferenceType
)
object ComponentMirror {
    val typename = "lucee.runtime.Component";
}

class StructMirror(
    val refType: ReferenceType
)
object StructMirror {
    val typename = "lucee.runtime.type.StructImpl";
}

class ArrayMirror(
    val refType : ReferenceType,
    val size: Method,
    val toArray: Method
)
object ArrayMirror {
    val typename = "lucee.runtime.type.ArrayImpl";
}

class FunctionMirror(
    val refType: ReferenceType
)
object FunctionMirror {
    val typename = "lucee.runtime.type.Closure";
}

class ArrowFunctionMirror(
    val refType: ReferenceType
)
object ArrowFunctionMirror {
    val typename = "lucee.runtime.type.Lambda"
}

class DoubleMirror (
    val refType: ReferenceType,
    val doubleValue: Method
)
object DoubleMirror {
    val typename = "java.lang.Double";
}