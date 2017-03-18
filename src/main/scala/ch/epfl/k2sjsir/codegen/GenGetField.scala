package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.Utils._
import org.jetbrains.kotlin.descriptors.PropertyDescriptor
import org.jetbrains.kotlin.ir.expressions.IrGetField
import org.scalajs.core.ir.Trees._

case class GenGetField(d: IrGetField, p: Positioner) extends Gen[IrGetField] {

  def tree: Tree = d.getDescriptor match {
    case pd: PropertyDescriptor =>
      val qual = GenExpr(d.getReceiver, p).tree
      Select(qual, pd.toIdent)(pd.getType.toJsType)
    case _ => notImplemented
  }

}
