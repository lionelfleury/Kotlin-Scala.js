package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.Utils._
import org.jetbrains.kotlin.descriptors.PropertyDescriptor
import org.jetbrains.kotlin.ir.declarations._
import org.scalajs.core.ir.Trees._

import scala.collection.JavaConversions._
import scala.language.implicitConversions

case class GenProperty(d: IrProperty, p: Positioner) extends Gen[IrProperty] {

  def tree: Tree = d.getDescriptor match {
    case pd: PropertyDescriptor =>
      val idt = pd.toIdent
      val g = Option(d.getGetter).map(getter)
      val s = Option(d.getSetter).map(setter)
      PropertyDef(idt, g, s)
    case _ => notImplemented
  }

  private def getter(f: IrFunction): Tree = GenBody(f.getBody, p).tree

  private def setter(f: IrFunction): (ParamDef, Tree) = {
    val desc = f.getDescriptor
    val body = getter(f)
    val param = desc.getValueParameters.map(_.toParamDef).get(0)
    (param, body)
  }

}
