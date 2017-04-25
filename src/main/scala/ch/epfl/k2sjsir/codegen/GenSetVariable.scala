package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.ir.expressions.IrSetVariable
import org.scalajs.core.ir.Trees._


case class GenSetVariable(d: IrSetVariable, p: Positioner) extends Gen[IrSetVariable] {

  def tree: Tree = {
    val a = d.getDescriptor
    val rhs = GenExpr(d.getValue, p).tree
    Assign(VarRef(a.toJsIdent)(a.getReturnType.toJsType), rhs)
  }

}