package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.PropertyDescriptor
import org.jetbrains.kotlin.ir.declarations._
import org.jetbrains.kotlin.resolve.DescriptorUtils
import org.scalajs.core.ir.Trees.{FieldDef, Tree}
import org.scalajs.core.ir.Types.{AnyType, NoType}

case class GenField(d: IrField, p: Positioner) extends Gen[IrField] {

  def tree: Tree = d.getDescriptor match {
    case pd: PropertyDescriptor =>
      val tpe = pd.getType.toJsType
      val idt = pd.toJsIdent
      val static = DescriptorUtils.isStaticDeclaration(pd)
      FieldDef(static, idt, tpe, pd.isVar)
    case _ => notImplemented
  }

}
