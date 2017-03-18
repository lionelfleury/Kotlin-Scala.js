package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.Utils._
import org.jetbrains.kotlin.descriptors.PropertyDescriptor
import org.jetbrains.kotlin.ir.declarations._
import org.scalajs.core.ir.Trees.{FieldDef, Tree}

case class GenField(d: IrField, p: Positioner) extends Gen[IrField] {

  def tree: Tree = d.getDescriptor match {
    case p: PropertyDescriptor =>
      FieldDef(p.toIdent, p.getReturnType.toJsType, p.isVar)
    case _ => notImplemented
  }

}
