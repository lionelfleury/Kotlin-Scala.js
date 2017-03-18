package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.Utils._
import org.jetbrains.kotlin.ir.declarations._
import org.scalajs.core.ir.Trees._


case class GenProperty(d: IrProperty, p: Positioner) extends Gen[IrProperty] {

  def tree: Tree = {
    val descriptor = d.getDescriptor
    val name = descriptor.getName.asString()
    val idt = Ident(name)
    val g = Option(d.getGetter)
    val s = Option(d.getSetter)
    val r = d.getBackingField
    PropertyDef(idt, g.map(getter), s.map(setter))
  }

  def getter(f: IrFunction): Tree = GenBody(f.getBody, p).tree

  def setter(f: IrFunction): (ParamDef, Tree) = {
    val body = getter(f)
    val desc = f.getDescriptor
    val tpe = desc.getReturnType.toJsType
    val param = f.getDescriptor.getValueParameters.get(0)
    val name = param.getName.asString()
    val idt = Ident(name)
    val p = ParamDef(idt, tpe, mutable = false, rest = false)
    (p, body)
  }

}
