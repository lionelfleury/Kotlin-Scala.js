package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.{AnyType, ArrayType, ClassType}

case class GenArrayOps(d: IrCall, p: Positioner, args: List[Tree]) extends Gen[IrCall] {

  import GenArrayOps._

  def tree: Tree = {
    val name = d.getDescriptor.toJsName
    if (specialArrays(name)) {
      NewArray(ArrayType(ClassType("O")), args)
    } else if (arrayFuns(name)) {
      args.head
    } else {
      val array = GenExpr(d.getDispatchReceiver, p).tree
      if (isGenericNext(d)) {
        val rtpe = d.getDescriptor.getReturnType.toJsRefType
        AsInstanceOf(Apply(array, Ident("next__O"), args)(AnyType), rtpe)
      } else name match {
        case "get" =>
          require(args.size == 1)
          val tpe = d.getDescriptor.getReturnType.toJsType
          ArraySelect(array, args.head)(tpe)
        case "set" =>
          require(args.size == 2)
          Assign(ArraySelect(array, args.head)(args(1).tpe), args(1))
        case "size" =>
          require(args.isEmpty)
          ArrayLength(array)
        case "iterator" =>
          val ctpe = d.getDescriptor.getReturnType.toJsClassType
          val (s, tpe) =
            if (ctpe.className == genericIterator) ("", "AO")
            else ("s", d.getDispatchReceiver.getType.toJsInternal)
          val clazz = ClassType(s"Lkotlin_jvm_internal_ArrayIterator${s}Kt")
          val method = Ident(s"iterator__${tpe}__${ctpe.className}")
          ApplyStatic(clazz, method, List(array))(ctpe)
        case _ => notImplemented
      }
    }
  }

}

object GenArrayOps {

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

  private def isGenericNext(d: IrCall): Boolean =
    d.getDispatchReceiver.getType.toJsInternal == genericIterator &&
      d.getDescriptor.getName.asString() == "next"

  def isArrayOps(d: IrCall): Boolean = {
    val name = d.getDescriptor.getName.asString()
    arrayFuns(name) || specialArrays(name) ||
      d.getDispatchReceiver.getType.toJsType.isInstanceOf[ArrayType] ||
      isGenericNext(d)
  }

}
