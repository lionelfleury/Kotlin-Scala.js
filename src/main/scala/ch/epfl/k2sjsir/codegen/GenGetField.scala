package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.ir.expressions.IrGetField
import org.scalajs.core.ir.Trees._

case class GenGetField(d: IrGetField, p: Positioner) extends Gen[IrGetField] {

  def tree: Tree = {
    val r = d.getReceiver
    val qual = if (r != null) GenExpr(r, p).tree else d.getDescriptor.getThis
    val idt = d.getDescriptor.toJsIdent
    val tpe = d.getType.toJsType
    Select(qual, idt)(tpe)
  }

}
