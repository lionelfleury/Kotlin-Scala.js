package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.ClassKind
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.utils.BindingUtils.getFunctionDescriptor
import org.jetbrains.kotlin.psi.KtNamedFunction
import org.jetbrains.kotlin.resolve.DescriptorUtils
import org.scalajs.core.ir.Trees._

import scala.collection.JavaConverters._

case class GenFun(d: KtNamedFunction)(implicit val c: TranslationContext) extends IRNodeGen[KtNamedFunction, MethodDef] {

  val desc = getFunctionDescriptor(c.bindingContext(), d)
  val cd = DescriptorUtils.getContainingClass(desc)

  override def tree: MethodDef = {
    val body = GenBody(d.getBodyExpression).treeOption
    val idt = desc.toJsMethodDeclIdent
    val extensionParam = Option(desc.getExtensionReceiverParameter).map(_.toJsParamDef)
    // See org.jetbrains.kotlin.backend.jvm.codegen.FunctionCodegen.kt line 56
    val interfaceParam = {
      if (isInterface)
        Option(ParamDef(Ident("$this"), cd.toJsClassType, false, false))
      else
        None
    }.toList
    val args = interfaceParam ++ extensionParam ++ desc.getValueParameters.asScala.map(_.toJsParamDef)
    val opt = OptimizerHints.empty.withInline(desc.isInline)
    val static = DescriptorUtils.isStaticDeclaration(desc) || isInterface
    MethodDef(static, idt, args, desc.getReturnType.toJsType, body)(opt, None)
  }

  def withAbstractBody: MethodDef = {
    val desc = getFunctionDescriptor(c.bindingContext(), d)
    val body = None
    val idt = desc.toJsMethodDeclIdent
    val extensionReceiver = Option(desc.getExtensionReceiverParameter).map(_.toJsParamDef)
    val args = extensionReceiver ++ desc.getValueParameters.asScala.map(_.toJsParamDef)
    val opt = OptimizerHints.empty.withInline(desc.isInline)
    val static = DescriptorUtils.isStaticDeclaration(desc)
    MethodDef(static, idt, args.toList, desc.getReturnType.toJsType, body)(opt, None)
  }

  private def isInterface: Boolean = {
    cd != null && cd.getKind == ClassKind.INTERFACE
  }

}
