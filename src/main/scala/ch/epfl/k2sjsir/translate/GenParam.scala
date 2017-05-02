package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.VariableDescriptor
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.utils.BindingUtils
import org.jetbrains.kotlin.psi.KtParameter
import org.jetbrains.kotlin.resolve.DescriptorUtils
import org.jetbrains.kotlin.types.TypeUtils
import org.scalajs.core.ir.Trees
import org.scalajs.core.ir.Trees.{Ident, ParamDef}
import org.scalajs.core.ir.Types.AnyType

case class GenParam(d: KtParameter)(implicit val c: TranslationContext) extends Gen[KtParameter] {

  override def tree: Trees.Tree = {
    val name = Ident(d.getName)
    val ptpe = BindingUtils.getDescriptorForElement(c.bindingContext(), d).asInstanceOf[VariableDescriptor]
    val tpe = ptpe.getType.toJsType
    ParamDef(name, tpe, d.isMutable, rest = false)
  }

}
