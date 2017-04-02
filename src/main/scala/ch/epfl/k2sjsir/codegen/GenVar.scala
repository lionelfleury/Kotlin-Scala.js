package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.VariableDescriptor
import org.jetbrains.kotlin.ir.declarations.IrVariable
import org.scalajs.core.ir.Trees._

case class GenVar(d: IrVariable, p: Positioner) extends Gen[IrVariable] {

  def tree: Tree = d.getDescriptor match {
    case v: VariableDescriptor =>
      val rhs = GenExpr(d.getInitializer, p).tree
      VarDef(v.toJsIdent, v.getReturnType.toJsType, v.isVar, rhs)
    case _ => notImplemented
  }

}
