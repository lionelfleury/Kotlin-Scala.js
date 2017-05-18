package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.CallableDescriptor
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.psi.KtCallExpression
import org.jetbrains.kotlin.resolve.calls.callUtil.CallUtilKt
import org.scalajs.core.ir.Trees
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.ArrayType

case class GenArray(d: KtCallExpression, args: List[Tree])(implicit val c: TranslationContext) extends Gen[KtCallExpression] {
  import GenArray._

  private val resolved = CallUtilKt.getResolvedCall(d, c.bindingContext())
  private val desc = resolved.getResultingDescriptor

  override def tree: Trees.Tree = {
    val name = desc.getName.asString()
    if (specialArrays(name)) {
      val tpe = desc.getReturnType.toJsArrayType
      if (args.isEmpty) ArrayValue(tpe, Nil)
      else NewArray(tpe, args)
    } else if (arrayFuns(name)) {
      val tpe = desc.getReturnType.toJsArrayType
      ArrayValue(tpe, args)
    } else {
      notImplemented
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

  private def isGenericNext(d: CallableDescriptor): Boolean =
    d.getDispatchReceiverParameter.getType.toJsInternal == genericIterator &&
      d.getName.asString() == "next"

  def isArrayOps(d: CallableDescriptor): Boolean = {
    val name = d.getName.asString()
    arrayFuns(name) || specialArrays(name) || (d.getDispatchReceiverParameter.getValue != null &&
      (d.getDispatchReceiverParameter.getType.toJsType.isInstanceOf[ArrayType] ||
        isGenericNext(d)))
  }

}
