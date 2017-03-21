package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.Utils._
import org.jetbrains.kotlin.descriptors.impl.LocalVariableDescriptor
import org.jetbrains.kotlin.ir.declarations.IrVariable
import org.scalajs.core.ir.Trees.{Tree, _}

case class GenVar(d: IrVariable, p: Positioner) extends Gen[IrVariable] {
  override def tree: Tree = d.getDescriptor match {
    case ldesc : LocalVariableDescriptor =>
      val rhs = GenExpr(d.getInitializer, p).tree
      val mutable = ldesc.isVar
      VarDef(ldesc.toIdent, rhs.tpe, mutable = mutable, rhs)
    case _ => notImplemented
  }
}
