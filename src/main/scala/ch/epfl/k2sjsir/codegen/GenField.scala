package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.descriptors.PropertyDescriptor
import org.jetbrains.kotlin.ir.declarations._
import org.scalajs.core.ir.{Trees => js}


case class GenField[T <: IrDeclaration](d: T, p: Positioner) extends AbstractGenerator[T](d, p) {

  def generate(): js.Tree = {
    val descriptor = d.getDescriptor.asInstanceOf[PropertyDescriptor]
    val name = descriptor.getName.asString()
    val tpe = descriptor.getReturnType.toJsType
    val idt = js.Ident(name)
    js.FieldDef(idt, tpe, descriptor.isVar)
  }

}
