package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.ClassDescriptor
import org.jetbrains.kotlin.descriptors.ClassKind.OBJECT
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.utils.AnnotationsUtils
import org.jetbrains.kotlin.js.translate.utils.BindingUtils._
import org.jetbrains.kotlin.psi._
import org.scalajs.core.ir.ClassKind
import org.scalajs.core.ir.Trees._

import scala.collection.immutable.{List, Nil}

case class GenExternalClass(d: KtClassOrObject)(implicit val c: TranslationContext) extends IRNodeGen[KtClassOrObject, ClassDef] {

  private val desc = getClassDescriptor(c.bindingContext(), d)
  private val optimizerHints = OptimizerHints.empty

  override def tree: ClassDef = {
    val idt = desc.toJsClassIdent
    val kind = if (isModule(desc)) ClassKind.NativeJSModuleClass else ClassKind.NativeJSClass

    val jsName = AnnotationsUtils.getJsName(desc)
    val hasAnnotation = Option(jsName).isDefined

    val name =
      if (hasAnnotation)
        jsName
      else if(isModule(desc))
        desc.getContainingDeclaration.getName.asString()
      else
        desc.getName.asString()

    val jsNativeLoadSpec = Some(JSNativeLoadSpec.Global(name, Nil))

    ClassDef(idt, kind, None, Some(Ident("sjs_js_Object")), List(), None, jsNativeLoadSpec, List(), Nil)(optimizerHints)
  }

  private def isModule(c: ClassDescriptor): Boolean = c.getKind == OBJECT
}
