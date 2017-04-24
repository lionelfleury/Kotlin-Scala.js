package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.Trees._

case class GenExpr(d: IrExpression, p: Positioner) extends Gen[IrExpression] {

  def tree: Tree = d match {
    case g: IrGetField => GenGetField(g, p).tree
    case c: IrCall => GenCall(c, p).tree
    case c: IrConst[_] => GenConst(c, p).tree
    case g: IrGetValue => GenGetValue(g, p).tree
    case b: IrBlock => GenBlock(b, p).tree
    case i: IrWhen => GenWhen(i, p).tree
    case e: IrGetObjectValue =>
      val ctpe = e.getDescriptor.toJsClassType
      LoadModule(ctpe)
    case t: IrTypeOperatorCall => GenTypeOp(t, p).tree
    case c: IrCallableReference => GenClosure(c, p).tree
    case r: IrReturn => GenExpr(r.getValue, p).tree
    case t: IrThrow => Throw(GenExpr(t.getValue, p).tree)
    case s: IrStringConcatenation => GenStringConcat(s, p).tree
    case _ => notImplemented
  }

}
