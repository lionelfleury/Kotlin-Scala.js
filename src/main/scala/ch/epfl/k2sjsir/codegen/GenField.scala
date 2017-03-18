package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.Utils._
import org.jetbrains.kotlin.ir.declarations._
import org.scalajs.core.ir.{Trees => js}

case class GenField(d: IrField, p: Positioner) extends Gen[IrField] {

  def tree: js.Tree = {
    val descriptor = d.getDescriptor
    val name = descriptor.getName.asString()
    val tpe = descriptor.getReturnType.toJsType
    val idt = js.Ident(name)
    js.FieldDef(idt, tpe, descriptor.isVar)
  }

}
