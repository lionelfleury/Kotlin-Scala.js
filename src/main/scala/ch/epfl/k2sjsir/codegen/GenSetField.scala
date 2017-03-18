package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.Utils._
import org.jetbrains.kotlin.descriptors.{ConstructorDescriptor, PropertyDescriptor}
import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.NoType

case class GenSetField(d: IrSetField, p: Positioner) extends Gen[IrSetField] {

  def tree: Tree = d.getDescriptor match {
    case cd: ConstructorDescriptor =>
      LoadModule(cd.getConstructedClass.toJsClassType)
    case pd: PropertyDescriptor =>
      val r = d.getReceiver
      val qual = if (r != null) GenExpr(r, p).tree else This()(NoType)
      val lhs = Select(qual, pd.toIdent)(pd.getType.toJsType)
      val rhs = GenExpr(d.getValue, p).tree
      Assign(lhs, rhs)
    case _ => notImplemented
  }

}
