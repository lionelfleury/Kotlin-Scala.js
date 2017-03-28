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
    UnaryOp(convertToOp(from, to), GenExpr(d.getDispatchReceiver, p).tree)
  }

}

object GenUnaryOp {

  def isUnaryOp(n: String): Boolean =
    n == "toLong" || n == "toInt" || n == "toDouble" || n == "toFloat"

  /* Useful for explicit type conversion (toInt, toDouble, ...) */
  private def convertToOp(from: Type, to: Type) = {
    (from, to) match {
      case (IntType, LongType) => UnaryOp.IntToLong
      case (LongType, IntType) => UnaryOp.LongToInt
      case (LongType, DoubleType) => UnaryOp.LongToDouble
      case (DoubleType, IntType) => UnaryOp.DoubleToInt
      case (DoubleType, FloatType) => UnaryOp.DoubleToFloat
      case (DoubleType, LongType) => UnaryOp.DoubleToLong
      case _ => throw new Exception("Unsupported type conversion")
    }
  }

}
