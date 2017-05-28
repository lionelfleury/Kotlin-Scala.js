package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.ClassKind.OBJECT
import org.jetbrains.kotlin.descriptors.{ClassConstructorDescriptor, ClassDescriptor, PropertyDescriptor}
import org.jetbrains.kotlin.js.translate.utils.PsiUtils.getPrimaryConstructorParameters
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.utils.BindingUtils._
import org.jetbrains.kotlin.psi._
import org.jetbrains.kotlin.resolve.DescriptorUtils
import org.jetbrains.kotlin.resolve.calls.callUtil.CallUtilKt
import org.jetbrains.kotlin.resolve.descriptorUtil.DescriptorUtilsKt._
import org.scalajs.core.ir.ClassKind
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.{AnyType, ClassType, NoType}

import scala.collection.JavaConverters._
import scala.collection.immutable.{List, Nil}

case class GenExternalClass(d: KtClassOrObject)(implicit val c: TranslationContext) extends Gen[KtClassOrObject] {

  private val desc = getClassDescriptor(c.bindingContext(), d)
  private val optimizerHints = OptimizerHints.empty

  override def tree: ClassDef = {
    val idt = desc.toJsClassIdent
    val kind = if (isModule(desc)) ClassKind.NativeJSModuleClass else ClassKind.NativeJSClass
    val name = if(isModule(desc)) desc.getContainingDeclaration.getName.asString() else desc.getName.asString()
    val jsNativeLoadSpec = Some(JSNativeLoadSpec.Global(List(name)))

    ClassDef(idt, kind, Some(Ident("sjs_js_Object")), List(), jsNativeLoadSpec, List())(optimizerHints)
  }

  private def isModule(c: ClassDescriptor): Boolean = c.getKind == OBJECT
}
