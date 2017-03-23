package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.{ConstructorDescriptor, PropertyDescriptor}
import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.Trees._

case class GenSetField(d: IrSetField, p: Positioner) extends Gen[IrSetField] {

  def tree: Tree = d.getDescriptor match {
    case cd: ConstructorDescriptor =>
      val tpe = cd.getConstructedClass.toJsClassType
      LoadModule(tpe)
    case pd: PropertyDescriptor =>
      val rhs = GenExpr(d.getValue, p).tree
      val r = d.getReceiver
      val qual = if (r!= null) GenExpr(r, p).tree else pd.getThis
      val lhs = Select(qual, pd.toJsIdent)(pd.getReturnType.toJsType)
      Assign(lhs, rhs)
    case _ => notImplemented
  }

}
