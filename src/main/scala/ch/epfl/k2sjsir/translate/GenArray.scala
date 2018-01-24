package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.CallableDescriptor
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.psi.KtCallExpression
import org.jetbrains.kotlin.resolve.calls.callUtil.CallUtilKt
import org.scalajs.core.ir.{Definitions, Trees, Types}
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types._

case class GenArray(d: KtCallExpression, args: List[Tree])(implicit val c: TranslationContext) extends Gen[KtCallExpression] {
  import GenArray._

  private val resolved = CallUtilKt.getResolvedCall(d, c.bindingContext())
  private val desc = resolved.getResultingDescriptor

  override def tree: Trees.Tree = {
    val name = desc.getName.asString()
    val tpe = desc.getReturnType.toJsArrayType

    if (specialArrays(name)) {
      if (args.isEmpty) ArrayValue(tpe, Nil)
      else NewArray(tpe, args)
    } else if (arrayFuns(name)) {
      ArrayValue(tpe, args)
    } else if (isNewKtArray(desc)) {
      // TODO: Remove this when using stdlib
      val size = args.head match {
        case IntLiteral(i) => i
        case _ => -1
      }
      val elementTpe = getPrimitiveArrayElementType(tpe.arrayTypeRef.baseClassName)
      val zeros = for(i <- 0 until size) yield Types.zeroOf(elementTpe)

      if (size < 0) {
        NewArray(tpe, args)
      } else {
        ArrayValue(tpe, zeros.toList)
      }
    } else {
      notImplemented()
    }
  }

}

object GenArray {

  private val genericIterator = "Lkotlin_collections_Iterator"
  private val specialArrays = Set("arrayOfNulls", "emptyArray")
  private val arrayFuns = Set(
    "arrayOf",
    "booleanArrayOf",
    "byteArrayOf",
    "charArrayOf",
    "doubleArrayOf",
    "floatArrayOf",
    "intArrayOf",
    "longArrayOf",
    "shortArrayOf"
  )

  private val kotlinArrays = Set(
    "IntArray",
    "BooleanArray",
    "ByteArray",
    "CharArray",
    "DoubleArray",
    "FloatArray",
    "LongArray",
    "ShortArray"
  )

  private def isGenericNext(d: CallableDescriptor): Boolean =
    d.getDispatchReceiverParameter.getType.toJsInternal == genericIterator &&
      d.getName.asString() == "next"

  private def getPrimitiveArrayElementType(s: String): Type = s match {
    case "Z" => BooleanType
    case "I" => IntType
    case "J" => LongType
    case "F" => FloatType
    case "D" => DoubleType
    case "T" => StringType
    case "B" => ByteType
    case "S" => ShortType
    case "C" => CharType
  }

  def isNewKtArray(d: CallableDescriptor): Boolean = {
    val name = d.getContainingDeclaration.getName.asString
    kotlinArrays(name)
  }

  def isArrayOps(d: CallableDescriptor): Boolean = {
    val name = d.getName.asString()
    val rcv = d.getDispatchReceiverParameter
    arrayFuns(name) || specialArrays(name) || (rcv != null && rcv.getValue != null &&
      (d.getDispatchReceiverParameter.getType.toJsType.isInstanceOf[ArrayType] ||
        isGenericNext(d)))
  }

}
