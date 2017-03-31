package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.Trees.{LoadModule, Tree, _}

case class GenExpr(d: IrExpression, p: Positioner) extends Gen[IrExpression] {

  def tree: Tree = d match {
    case g: IrGetField => GenGetField(g, p).tree
    case c: IrCall => GenCall(c, p).tree
    case c: IrConst[_] => GenConst(c, p).tree
    case g: IrGetValue => GenGetValue(g, p).tree
    case b: IrBlock => GenBlock(b, p).tree
    case i: IrWhen => GenWhen(i, p).tree
    case s: IrStringConcatenation => GenString(s, p).tree
    case e: IrGetObjectValue =>
      val ctpe = e.getDescriptor.toJsClassType
      LoadModule(ctpe)
    case t: IrTypeOperatorCall =>
      val arg = GenExpr(t.getArgument, p).tree
      val y = t.getOperator // TODO: For example IMPLICIT_NOTNULL
      arg
    case c: IrCallableReference =>
      val ref = c.getDescriptor.toJsIdent
      VarRef(ref)(c.getDescriptor.getReturnType.toJsType)
    case null => notImplemented
    case _ => notImplemented
  }

}
