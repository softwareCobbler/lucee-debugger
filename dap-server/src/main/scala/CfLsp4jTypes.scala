package CfDebugAdapter

import org.eclipse.lsp4j.debug.*;
import com.sun.jdi.ReferenceType;
import java.util.ArrayList
import scala.util.matching.Regex

object ConstantPoolHelper {
    val CONSTANT_Class              = 7;
    val CONSTANT_Fieldref           = 9;
    val CONSTANT_Methodref          = 10;
    val CONSTANT_InterfaceMethodref = 11;
    val CONSTANT_String             = 8;
    val CONSTANT_Integer            = 3;
    val CONSTANT_Float              = 4;
    val CONSTANT_Long               = 5;
    val CONSTANT_Double             = 6;
    val CONSTANT_NameAndType        = 12;
    val CONSTANT_Utf8               = 1;
    val CONSTANT_MethodHandle       = 15;
    val CONSTANT_MethodType         = 16;
    val CONSTANT_InvokeDynamic      = 18;

    val structureLengths = Map(
        (CONSTANT_Class,              3),
        (CONSTANT_Fieldref,           5),
        (CONSTANT_Methodref,          5),
        (CONSTANT_InterfaceMethodref, 5),
        (CONSTANT_String,             3),
        (CONSTANT_Integer,            5),
        (CONSTANT_Float,              5),
        (CONSTANT_Long,               9), // note: takes 2 pool slots
        (CONSTANT_Double,             9), // note: takes 2 pool slots
        (CONSTANT_NameAndType,        5),
        (CONSTANT_Utf8,               -1), // dynamic length based on data
        (CONSTANT_MethodHandle,       4),
        (CONSTANT_MethodType,         3),
        (CONSTANT_InvokeDynamic,      5),
    )

    /**
     * the length of the entire utf8 structure, including tag byte, both length bytes, and data segment
     * i.e.
     * struct {
     *     uint8_t tag;
     *     uint16_t length;
     *     uint8_t data[];
     * }
     */
    def utf8Length(bytes: Array[Byte], indexOfUtf8TagByte: Int) : Int = {
        val dataSegmentLength =
            ((bytes(indexOfUtf8TagByte + 1) & 0x000000FF) << 8) |
            ((bytes(indexOfUtf8TagByte + 2) & 0x000000FF) << 0);
        return 3 + dataSegmentLength;
    }
}

// remap with a from/to glob pattern like "/abc/*" -> "/xyz/*",
// a path like "/abc/index.cfm" becomes "/xyz/index.cfm"
class PathGlobRemapper(val from: String, to: String) {
    private val isValid = {
        def hasOneGlob(s: String) = s.indexOf("*") == s.lastIndexOf("*");
        hasOneGlob(from) && hasOneGlob(to);
    }

    private var pattern = {
        if isValid
        then Some(("^" + from.replace("*", "(.*?)") + "$").r);
        else None
    }

    def remap(path: String) = {
        pattern match {
            case Some(regex) => {
                path match {
                    case regex(globPart) => to.replace("*", globPart);
                    case _ => path
                }
            }
            case None => path;
        }
    }

    private def forceIdentityRemap = pattern = None;
}
object PathGlobRemapper {
    def identityRemapper : PathGlobRemapper = {
        val result = PathGlobRemapper("","");
        result.forceIdentityRemap;
        result;
    }
}

class CfSourceFileWrapper(val absPath: String, val refType: ReferenceType) {
    lazy val compileTime : Long = getCompileTime();
    // originalSourceAbsPath
    // className
    // 

    /**
     * we expect that (for lucee at least) there is a method
     * `long getCompileTime() { return <constexpr>; }`
     * 
     * where the bytecode is:
     *   ldc2_w indexbyte1 indexbyte2
     *   lreturn
     */
    private def getCompileTime() : Long = {
        import ConstantPoolHelper.structureLengths;

        val methodListing = refType.methodsByName("getCompileTime", "()J");
        val method = methodListing.get(0);
        val bytecodes = method.bytecodes();

        val LDC2_W = 0x14;

        if (bytecodes.length >= 4) {
            if (bytecodes(0) == LDC2_W) {
                val indexbyte1 = bytecodes(1)
                val indexbyte2 = bytecodes(2)
                val targetPoolIndex =
                    ((indexbyte1 & 0x000000FF) << 8) |
                    ((indexbyte2 & 0X000000FF) << 0)
                val poolBytes = refType.constantPool()

                var i = 0;
                var poolIndex = 1;

                while (poolIndex != targetPoolIndex) {
                    val tag = poolBytes(i);
                    tag match {
                        case ConstantPoolHelper.CONSTANT_Utf8 => {
                            val length = ConstantPoolHelper.utf8Length(poolBytes, i);
                            i += length;
                            poolIndex += 1;
                        }
                        case ConstantPoolHelper.CONSTANT_Double => {
                            i += 9;
                            poolIndex += 2;
                        }
                        case ConstantPoolHelper.CONSTANT_Long => {
                            i += 9;
                            poolIndex += 2;
                        }
                        case _ => {
                            ConstantPoolHelper.structureLengths.get(tag) match {
                                case Some(length) => {
                                    i += length;
                                    poolIndex += 1;
                                }
                                case None => return 0L; // error case?
                            }
                        }
                    }
                }

                i += 1; // move past constant pool's type tag (which says "hey the next 8 bytes constitute a LONG")

                val b0 = (poolBytes(i+0) & 0x00000000000000FFL) << 56;
                val b1 = (poolBytes(i+1) & 0x00000000000000FFL) << 48;
                val b2 = (poolBytes(i+2) & 0x00000000000000FFL) << 40;
                val b3 = (poolBytes(i+3) & 0x00000000000000FFL) << 32;
                val b4 = (poolBytes(i+4) & 0x00000000000000FFL) << 24;
                val b5 = (poolBytes(i+5) & 0x00000000000000FFL) << 16;
                val b6 = (poolBytes(i+6) & 0x00000000000000FFL) << 8;
                val b7 = (poolBytes(i+7) & 0x00000000000000FFL) << 0;

                return b0 | b1 | b2 | b3 | b4 | b5 | b6 | b7;
            }
        }

        return 0L; // error case?
    }
}