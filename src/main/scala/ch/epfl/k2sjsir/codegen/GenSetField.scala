package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.{ConstructorDescriptor, PropertyDescriptor}
import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.Trees._

case class GenSetField(d: IrSetField, p: Positioner) extends Gen[IrSetField] {

  def tree: Tree = d.getDescriptor match {
    case cd: ConstructorDescriptor =>
      LoadModule(cd.getConstructedClass.toJsClassType)
    case pd: PropertyDescriptor =>
      val r = d.getReceiver
      val rhs = GenExpr(d.getValue, p).tree
      val qual = if (r != null) GenExpr(r, p).tree else This()(rhs.tpe)
      val lhs = Select(qual, pd.toJsIdent)(rhs.tpe)
      Assign(lhs, rhs)
    case _ => notImplemented
  }

}
