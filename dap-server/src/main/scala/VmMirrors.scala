/**
 * "mirrors" (jdi terminology) representing object types in the target VM
 * the assumption is that there is only a small amount of which we are interested in, so we can "declare" them here
 * the CfVm is responsible for scanning the target VM and making an instance of each of these, if it wants to invoke methods or etc. on them
 */

package CfDebugAdapter;

import com.sun.jdi.ClassType;
import com.sun.jdi.InterfaceType;
import com.sun.jdi.ObjectReference;
import com.sun.jdi.ReferenceType;
import com.sun.jdi.ClassObjectReference;
import com.sun.jdi.Method;
import com.sun.jdi.ThreadReference;
import com.sun.jdi.Value;
import java.util.ArrayList

// ClassUtil serves to load non-page class files on the target VM
class ClassUtilMirror (
    val classType: ClassType,
    val loadClass: Method,
)
object ClassUtilMirror {
    val typename = "lucee.commons.lang.ClassUtil";
}

trait TypeMirror {
    val referenceType: ReferenceType;
    def classObject : ClassObjectReference = referenceType.classObject();
}

class MapMirror(
    val referenceType: ReferenceType,
    val keySet: Method,
    val get: Method,
    val size: Method,
) extends TypeMirror;
object MapMirror {
    val typename = "java.util.Map";
}

class SetMirror(
    val referenceType: ReferenceType,
    val toArray: Method
) extends TypeMirror;
object SetMirror {
    val typename = "java.util.Set";
}

// compiled page hierarchy:
// TOP -> lucee.runtime.Page -> lucee.runtime.PageImpl -> (actual compiled cf class)
// so this represents a compiled CF source file
class PageImplMirror (
    val referenceType: ReferenceType,
) extends TypeMirror;
object PageImplMirror {
    val typename = "lucee.runtime.PageImpl";
}

class ComponentPageImplMirror(
    val referenceType: ReferenceType,
) extends TypeMirror;
object ComponentPageImplMirror {
    val typename = "lucee.runtime.ComponentPageImpl";
}

class PageBaseMirror(
    val referenceType: ReferenceType,
    val getCompileTime: Method
) extends TypeMirror;
object PageBaseMirror { // abstract base class of all cf-generated page classes
    val typename = "lucee.runtime.Page";
}

// PageContext is where the cf engine places runtime information for things like the page's variables scope, and etc.
// TOP -> javax.servlet.jsp.PageContext -> PageContext -> PageContextImpl
class PageContextImplMirror(
    val referenceType: ReferenceType,
    val variablesScope: Method,
    val localScope: Method,
    val argumentsScope: Method,
    val getActiveUdf: Method
) extends TypeMirror;
object PageContextImplMirror {
    val typename = "lucee.runtime.PageContextImpl";
}

class ClassMirror(
    val referenceType: ReferenceType,
    val isAssignableFrom: Method
) extends TypeMirror;
object ClassMirror {
    val typename = "java.lang.Class";
}

//
// cf runtime type mirrors
// strings are raw Strings, and so are mirrored by jdi StringReference
// numbers are raw Doubles, and so are mirrored by ... ?
//

class ThreadLocalPageContextMirror(
    val referenceType: ReferenceType,
    val get: Method // static
) extends TypeMirror;
object ThreadLocalPageContextMirror {
    val typename = "lucee.runtime.engine.ThreadLocalPageContext"
}

class ComponentMirror(
    val referenceType: ReferenceType,
) extends TypeMirror;
object ComponentMirror {
    val typename = "lucee.runtime.Component";
}

// everything's just a Map[K,V]
// class StructMirror(
//     val classRef: ClassObjectReference
// )
// object StructMirror {
//     val typename = "lucee.runtime.type.StructImpl";
// }

// could probably get away with using AbstractList[E]?
class ArrayMirror(
    val referenceType: ReferenceType,
    val size: Method,
    val toArray: Method
) extends TypeMirror;
object ArrayMirror {
    val typename = "lucee.runtime.type.ArrayImpl";
}

class FunctionMirror(
    val referenceType: ReferenceType,
) extends TypeMirror;
object FunctionMirror {
    val typename = "lucee.runtime.type.Closure";
}

class ArrowFunctionMirror(
    val referenceType: ReferenceType,
) extends TypeMirror;
object ArrowFunctionMirror {
    val typename = "lucee.runtime.type.Lambda"
}

class DoubleMirror (
    val referenceType: ReferenceType,
    val doubleValue: Method
) extends TypeMirror;
object DoubleMirror {
    val typename = "java.lang.Double";
}

class JvmMirrors(threadRef: ThreadReference, classUtil: ClassUtilMirror) {
    private def loadClass(typename: String) : ReferenceType = {
        val args = ArrayList[Value]();
        args.add(threadRef.virtualMachine.mirrorOf(typename));
        val classObjectRef = classUtil
            .classType
            .invokeMethod(threadRef, classUtil.loadClass, args, ClassType.INVOKE_SINGLE_THREADED)
            .asInstanceOf[ClassObjectReference];
        return classObjectRef.reflectedType;
    }

    val map : MapMirror = {
        val refType = loadClass(MapMirror.typename);
        val keySet = refType.methodsByName("keySet");
        val get = refType.methodsByName("get");
        val size = refType.methodsByName("size");
        MapMirror(referenceType = refType, keySet = keySet.get(0), get = get.get(0), size = size.get(0));
    }

    val set : SetMirror = {
        val refType = loadClass(SetMirror.typename);
        val toArray = refType.methodsByName("toArray", "()[Ljava/lang/Object;");
        SetMirror(referenceType = refType, toArray = toArray.get(0));
    }

    val klass : ClassMirror = {
        val refType = loadClass(ClassMirror.typename);
        val isAssignableFrom = refType.methodsByName("isAssignableFrom");
        ClassMirror(referenceType = refType, isAssignableFrom = isAssignableFrom.get(0));
    }

    val pageBase: PageBaseMirror = {
        val refType = loadClass(PageBaseMirror.typename);
        val getCompileTime = refType.methodsByName("getCompileTime");
        PageBaseMirror(referenceType = refType, getCompileTime = getCompileTime.get(0));
    }

    val pageImpl : PageImplMirror = {
        val refType = loadClass(PageImplMirror.typename);
        PageImplMirror(referenceType = refType);
    }

    val componentPageImpl : ComponentPageImplMirror = {
        val refType = loadClass(ComponentPageImplMirror.typename);
        ComponentPageImplMirror(referenceType = refType);
    }

    val threadLocalPageContext : ThreadLocalPageContextMirror = {
        val refType = loadClass(ThreadLocalPageContextMirror.typename);
        val get = refType.methodsByName("get", "()Llucee/runtime/PageContext;")
        ThreadLocalPageContextMirror(referenceType = refType, get = get.get(0));
    }

    val pageContextImpl : PageContextImplMirror = {
        val refType = loadClass(PageContextImplMirror.typename);
        val variablesScope = refType.methodsByName("variablesScope");
        val argumentsScope = refType.methodsByName("argumentsScope");
        val localScope = refType.methodsByName("localScope");
        val getActiveUdf = refType.methodsByName("getActiveUDF");
        PageContextImplMirror(
            referenceType = refType,
            variablesScope = variablesScope.get(0),
            localScope = localScope.get(0),
            argumentsScope = argumentsScope.get(0),
            getActiveUdf = getActiveUdf.get(0)
        );
    }

    val component : ComponentMirror = {
        val refType = loadClass(ComponentMirror.typename);
        ComponentMirror(referenceType = refType);
    }

    val array : ArrayMirror = {
        val refType = loadClass(ArrayMirror.typename);
        val size = refType.methodsByName("size");
        val toArray = refType.methodsByName("toArray", "()[Ljava/lang/Object;");
        ArrayMirror(referenceType = refType, size= size.get(0), toArray=toArray.get(0));
    }

    val function : FunctionMirror = {
        val refType = loadClass(FunctionMirror.typename);
        FunctionMirror(referenceType = refType);
    }

    val arrowFunction : ArrowFunctionMirror = {
        val refType = loadClass(ArrowFunctionMirror.typename);
        ArrowFunctionMirror(referenceType = refType);
    }

    val double : DoubleMirror = {
        val refType = loadClass(DoubleMirror.typename);
        val doubleValue = refType.methodsByName("doubleValue");
        DoubleMirror(referenceType = refType, doubleValue = doubleValue.get(0));
    }
}
