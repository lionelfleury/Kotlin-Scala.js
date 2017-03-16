package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.declarations._
import org.scalajs.core.ir.{Trees => js}


case class GenProperty[T <: IrProperty](d: T, p: Positioner) extends AbstractGenerator[T](d, p) {

  def generate(): js.Tree = {
    val descriptor = d.getDescriptor
    val name = descriptor.getName.asString()
    val idt = js.Ident(name)
    val g = d.getGetter
    val s = d.getSetter
    js.PropertyDef(idt, genGetter(g), genSetter(s))
  }

  def genGetter(g: IrFunction): Option[js.Tree] = {
    if (g == null) None
    else Some(GenBody(g.getBody, p).generate())
  }

  def genSetter(s: IrFunction): Option[(js.ParamDef, js.Tree)] = {
    if (s == null) None
    else s match {
      case _ =>
        println(s"Not supported Property Setter: $s")
        // TODO: Implement setter...
        None
    }
  }

}
