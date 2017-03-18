package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.Utils._
import org.jetbrains.kotlin.descriptors.PropertyDescriptor
import org.jetbrains.kotlin.ir.declarations._
import org.scalajs.core.ir.Trees.{FieldDef, Tree}
import org.scalajs.core.ir.Types.{AnyType, NoType}

case class GenField(d: IrField, p: Positioner) extends Gen[IrField] {

  def tree: Tree = d.getDescriptor match {
    case p: PropertyDescriptor =>
      val tpe = if (p.getReturnType != null) p.getReturnType.toJsType else AnyType
      val t = if (tpe == NoType) AnyType else tpe
      val idt = p.toIdent
      FieldDef(idt, t, p.isVar)
    case _ => notImplemented
  }

}
