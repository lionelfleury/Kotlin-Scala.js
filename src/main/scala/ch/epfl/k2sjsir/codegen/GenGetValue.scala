package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.{ParameterDescriptor, ValueDescriptor}
import org.jetbrains.kotlin.ir.expressions.IrGetValue
import org.scalajs.core.ir.Trees._

case class GenGetValue(d: IrGetValue, p: Positioner) extends Gen[IrGetValue] {

  def tree: Tree = d.getDescriptor match {
    case rp: ParameterDescriptor =>
      val tpe = rp.getReturnType.toJsType
      This()(tpe)
    case vd: ValueDescriptor =>
      val idt = vd.toJsIdent
      val tpe = vd.getReturnType.toJsType
      VarRef(idt)(tpe)
    case _ => notImplemented
  }

}
