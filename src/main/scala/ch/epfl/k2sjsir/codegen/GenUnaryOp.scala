package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.ir.expressions.IrCall
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types._

case class GenUnaryOp(d: IrCall, p: Positioner) extends Gen[IrCall] {

  import GenUnaryOp._

  def tree: Tree = {
    val sf = d.getDescriptor
    val from = sf.getDispatchReceiverParameter.getType.toJsType
    val to = sf.getReturnType.toJsType
    val converted = convertToOp(from, to)
    if(isUnaryToBinary(sf.getName.toString)) convertUnaryToBinary(sf.getName.toString, sf.getReturnType.toJsType)
    else
      converted.fold(GenExpr(d.getDispatchReceiver, p).tree)(UnaryOp(_, GenExpr(d.getDispatchReceiver, p).tree))
  }


  private def convertUnaryToBinary(name: String, tpe: Type) : Tree = (name, tpe) match {
    case ("unaryMinus", IntType) =>
      BinaryOp(GenBinaryOp.getBinaryOp("minus", tpe), IntLiteral(0), GenExpr(d.getDispatchReceiver, p).tree)
    case _ => Debugger()
  }
}

object GenUnaryOp {

  def isUnaryOp(n: String): Boolean =
    n == "toLong" || n == "toInt" || n == "toDouble" || n == "toFloat" || n == "not" || isUnaryToBinary(n)

  private def isUnaryToBinary(n: String) = n == "unaryMinus"

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
