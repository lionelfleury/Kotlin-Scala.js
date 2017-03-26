package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.ir.expressions.IrGetField
import org.jetbrains.kotlin.resolve.DescriptorUtils.{isStaticDeclaration, _}
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.ClassType

case class GenGetField(d: IrGetField, p: Positioner) extends Gen[IrGetField] {

  def tree: Tree = {
    val pd = d.getDescriptor
    val idt = pd.toJsIdent
    val tpe = d.getType.toJsType
    val static = isStaticDeclaration(pd)
    if (static) {
      val ctpe = getContainingClass(pd).toJsClassName + "$"
      SelectStatic(ClassType(ctpe), idt)(tpe)
    }
    else Select(GenExpr(d.getReceiver, p).tree, idt)(tpe)
  }

}
