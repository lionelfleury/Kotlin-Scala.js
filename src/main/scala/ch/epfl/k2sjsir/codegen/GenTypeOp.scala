package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.ir.expressions.IrTypeOperator._
import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.Trees._

case class GenTypeOp(d: IrTypeOperatorCall, p: Positioner) extends Gen[IrTypeOperatorCall] {

  def tree: Tree = {
    val exp = GenExpr(d.getArgument, p).tree
    val tpe = d.getTypeOperand.toJsRefType
    d.getOperator match {
      case CAST | IMPLICIT_CAST | SAFE_CAST | IMPLICIT_NOTNULL =>
        AsInstanceOf(exp, tpe)
      case INSTANCEOF =>
        IsInstanceOf(exp, tpe)
      case _ => notImplemented
    }
  }

}
