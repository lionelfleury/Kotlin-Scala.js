package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.ir.expressions.IrTypeOperator._
import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.ClassType

case class GenTypeOp(d: IrTypeOperatorCall, p: Positioner) extends Gen[IrTypeOperatorCall] {

  def tree: Tree = {
    val exp = GenExpr(d.getArgument, p).tree
    val tpe = ClassType(d.getTypeOperand.toJsInternal) //TODO: Don't know how to check
    d.getOperator match {
      case CAST | IMPLICIT_CAST | SAFE_CAST | IMPLICIT_NOTNULL =>
        AsInstanceOf(exp, tpe)
      case INSTANCEOF =>
        IsInstanceOf(exp, tpe)
      case _ => notImplemented
    }
  }

}
