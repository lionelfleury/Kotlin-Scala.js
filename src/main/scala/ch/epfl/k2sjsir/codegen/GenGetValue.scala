package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.Utils._
import org.jetbrains.kotlin.descriptors.impl.LazyClassReceiverParameterDescriptor
import org.jetbrains.kotlin.descriptors.{ClassDescriptor, ValueParameterDescriptor}
import org.jetbrains.kotlin.ir.expressions.IrGetValue
import org.scalajs.core.ir.Trees._

case class GenGetValue(d: IrGetValue, p: Positioner) extends Gen[IrGetValue] {

  def tree: Tree = d.getDescriptor match {
    case rp: LazyClassReceiverParameterDescriptor =>
      val cd = rp.getContainingDeclaration match {
        case c: ClassDescriptor => c
        case _ => throw new Error("Only class descriptor supported...")
      }
      val tpe = cd.toJsClassType
      This()(tpe)
    case vd: ValueParameterDescriptor =>
      VarRef(vd.toIdent)(vd.getReturnType.toJsType)
    case _ => notImplemented
  }

}
