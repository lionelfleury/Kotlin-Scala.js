package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.PropertyDescriptor
import org.jetbrains.kotlin.ir.declarations._
import org.jetbrains.kotlin.resolve.DescriptorUtils
import org.scalajs.core.ir.Trees._

import scala.collection.JavaConverters._

case class GenProperty(d: IrProperty, p: Positioner) extends Gen[IrProperty] {

  def tree: Tree = d.getDescriptor match {
    case pd: PropertyDescriptor =>
      val idt = pd.toJsIdent
      val g = Option(d.getGetter).map(getter)
      val s = Option(d.getSetter).map(setter)
      val static = DescriptorUtils.isStaticDeclaration(pd)
      PropertyDef(static, idt, g, s)
    case _ => notImplemented
  }

  private def getter(f: IrFunction): Tree = GenBody(f.getBody, p).tree

  private def setter(f: IrFunction): (ParamDef, Tree) = {
    val desc = f.getDescriptor
    val body = getter(f)
    val param = desc.getValueParameters.asScala.map(_.toJsParamDef).head
    (param, body)
  }

}
