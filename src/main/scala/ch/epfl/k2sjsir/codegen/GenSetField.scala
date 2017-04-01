package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.{ConstructorDescriptor, PropertyDescriptor}
import org.jetbrains.kotlin.ir.expressions._
import org.jetbrains.kotlin.resolve.DescriptorUtils.{getClassDescriptorForType, isStaticDeclaration}
import org.scalajs.core.ir.Trees._

case class GenSetField(d: IrSetField, p: Positioner) extends Gen[IrSetField] {

  def tree: Tree = d.getDescriptor match {
    case cd: ConstructorDescriptor =>
      val tpe = cd.getConstructedClass.toJsClassType
      LoadModule(tpe)
    case pd: PropertyDescriptor =>
      val rhs = GenExpr(d.getValue, p).tree
      val idt = pd.toJsIdent
      val static = isStaticDeclaration(pd)
      val lhs = if (static) {
        val ctpe = getClassDescriptorForType(pd.getType).toJsClassType
        SelectStatic(ctpe, idt)(rhs.tpe)
      } else Select(GenExpr(d.getReceiver, p).tree, idt)(rhs.tpe)
      Assign(lhs, rhs)
    case _ => notImplemented
  }

}
