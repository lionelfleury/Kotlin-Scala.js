package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.ir.expressions.IrCall
import org.scalajs.core.ir.Trees.{Tree, UnaryOp}
import org.scalajs.core.ir.Types._

case class GenUnaryOp(d: IrCall, p: Positioner) extends Gen[IrCall] {

  import GenUnaryOp.convertToOp

  def tree: Tree = {
    val sf = d.getDescriptor
    val from = sf.getDispatchReceiverParameter.getType.toJsType
    val to = sf.getReturnType.toJsType
    val converted = convertToOp(from, to)
    val arg = GenExpr(d.getDispatchReceiver, p).tree
    converted.fold(arg)(UnaryOp(_, arg))
  }

}

object GenUnaryOp {

  def isUnaryOp(n: String): Boolean =
    n == "toLong" || n == "toInt" || n == "toDouble" || n == "toFloat" || n == "not"

  /* Useful for explicit type conversion (toInt, toDouble, ...) */
  private def convertToOp(from: Type, to: Type) = (from, to) match {
    case (IntType, LongType) => Some(UnaryOp.IntToLong)
    case (LongType, IntType) => Some(UnaryOp.LongToInt)
    case (LongType, DoubleType) => Some(UnaryOp.LongToDouble)
    case (DoubleType, IntType) => Some(UnaryOp.DoubleToInt)
    case (DoubleType, FloatType) => Some(UnaryOp.DoubleToFloat)
    case (DoubleType, LongType) => Some(UnaryOp.DoubleToLong)
    case (BooleanType, BooleanType) => Some(UnaryOp.Boolean_!)
    case _ => None
  }

}
