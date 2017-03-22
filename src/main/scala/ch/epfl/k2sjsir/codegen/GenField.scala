package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.ir.declarations.IrField
import org.jetbrains.kotlin.resolve.DescriptorUtils.isStaticDeclaration
import org.scalajs.core.ir.Trees._

case class GenField(d: IrField, p: Positioner) extends Gen[IrField] {

  def tree: Tree = {
    val pd = d.getDescriptor
    val static = isStaticDeclaration(pd)
    val idt = pd.toJsIdent
    val tpe = pd.getType.toJsType
    FieldDef(static, idt, tpe, pd.isVar)
  }

}
