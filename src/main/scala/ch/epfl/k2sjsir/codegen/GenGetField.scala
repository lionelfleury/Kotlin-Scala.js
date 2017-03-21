package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.PropertyDescriptor
import org.jetbrains.kotlin.ir.expressions.IrGetField
import org.scalajs.core.ir.Trees._

case class GenGetField(d: IrGetField, p: Positioner) extends Gen[IrGetField] {

  def tree: Tree = d.getDescriptor match {
    case pd: PropertyDescriptor =>
      val qual = GenExpr(d.getReceiver, p).tree
      val idt = pd.toJsIdent
      val tpe = pd.getReturnType.toJsType
      Select(qual, idt)(tpe)
    case _ => notImplemented
  }

}
