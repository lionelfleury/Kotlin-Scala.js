package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.utils.BindingUtils.getFunctionDescriptor
import org.jetbrains.kotlin.psi.KtNamedFunction
import org.jetbrains.kotlin.resolve.DescriptorUtils
import org.scalajs.core.ir.Trees._

import scala.collection.JavaConverters._

case class GenFun(d: KtNamedFunction)(implicit val c: TranslationContext) extends Gen[KtNamedFunction] {

  override def tree: Tree = {
    val desc = getFunctionDescriptor(c.bindingContext(), d)
    val body = GenBody(d.getBodyExpression).treeOption
    val idt = desc.toJsMethodIdent
    val x = Option(desc.getExtensionReceiverParameter).map(_.toJsParamDef)
    val args = x ++ desc.getValueParameters.asScala.map(_.toJsParamDef)
    val opt = OptimizerHints.empty.withInline(desc.isInline)
    val static = DescriptorUtils.isStaticDeclaration(desc)
    MethodDef(static, idt, args.toList, desc.getReturnType.toJsType, body)(opt, None)
  }

}
