package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.declarations._
import org.scalajs.core.ir.{Trees => js}


case class GenField[T <: IrField](d: T, p: Positioner) extends AbstractGenerator[T](d, p) {

  def generate(): js.Tree = {
    val descriptor = d.getDescriptor
    val name = descriptor.getName.asString()
    val tpe = descriptor.getReturnType.toJsType
    val idt = js.Ident(name)
    js.FieldDef(idt, tpe, mutable = descriptor.isVar)
  }

}
