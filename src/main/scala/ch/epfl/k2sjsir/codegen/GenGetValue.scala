package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.ValueDescriptor
import org.jetbrains.kotlin.descriptors.impl.LazyClassReceiverParameterDescriptor
import org.jetbrains.kotlin.ir.expressions.IrGetValue
import org.jetbrains.kotlin.resolve.DescriptorUtils.getClassDescriptorForType
import org.scalajs.core.ir.Trees._

case class GenGetValue(d: IrGetValue, p: Positioner) extends Gen[IrGetValue] {

  def tree: Tree = d.getDescriptor match {
    case rp: LazyClassReceiverParameterDescriptor =>
      val ctpe = getClassDescriptorForType(rp.getType).toJsClassType
      This()(ctpe)
    case vd: ValueDescriptor =>
      val idt = vd.toJsIdent
      val tpe = vd.getReturnType.toJsType
      VarRef(idt)(tpe)
    case _ => notImplemented
  }

}
