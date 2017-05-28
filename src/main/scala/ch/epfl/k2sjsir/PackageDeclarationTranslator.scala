package ch.epfl.k2sjsir

import java.{util => ju}

import ch.epfl.k2sjsir.lower.{ClassLowering, SJSIRLower}
import ch.epfl.k2sjsir.translate.{GenClass, GenExternalClass, GenFun}
import ch.epfl.k2sjsir.utils.NameEncoder
import com.intellij.openapi.util.io.FileUtil
import org.jetbrains.kotlin.config.CommonConfigurationKeys
import org.jetbrains.kotlin.diagnostics.DiagnosticUtils
import org.jetbrains.kotlin.fileClasses.JvmFileClassUtil
import org.jetbrains.kotlin.js.facade.exceptions.TranslationRuntimeException
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.declaration.FileDeclarationVisitor
import org.jetbrains.kotlin.js.translate.general.AbstractTranslator
import org.jetbrains.kotlin.js.translate.utils.BindingUtils._
import org.jetbrains.kotlin.js.translate.utils.{AnnotationsUtils, BindingUtils}
import org.jetbrains.kotlin.psi._
import org.scalajs.core.ir.{ClassKind, Position}
import org.scalajs.core.ir.Trees.{ClassDef, Ident, OptimizerHints}

import scala.collection.JavaConversions._
import scala.collection.mutable


object PackageDeclarationTranslator {
  def translateFiles(files: ju.Collection[KtFile], context: TranslationContext): Unit = {
    new PackageDeclarationTranslator(files, context).translate()
  }
}

final class PackageDeclarationTranslator private(
                                                  val files: Iterable[KtFile],
                                                  override val context: TranslationContext
                                                ) extends AbstractTranslator(context) {
  private def translate(): Unit = {
    val output = context.getConfig.getConfiguration.get(CommonConfigurationKeys.MODULE_NAME)

    for (file <- files) {
      try {
        val topLevel = new mutable.MutableList[KtNamedFunction]()
        val declarations = new SJSIRLower().lower(file)
        for (declaration <- declarations) {
            val predefinedObject = AnnotationsUtils.isPredefinedObject(BindingUtils.getDescriptorForElement(bindingContext, declaration))
            declaration match {
              case d: KtClassOrObject if !predefinedObject=>
                val tree = GenClass(d)(context).tree
                val cd = getClassDescriptor(context.bindingContext(), d)
                SJSIRCodegen.genIRFile(output, cd, tree)
              case d: KtClassOrObject =>
                val tree = GenExternalClass(d)(context).tree
                val cd = getClassDescriptor(context.bindingContext(), d)
                SJSIRCodegen.genIRFile(output, cd, tree)
              case f: KtNamedFunction =>
                topLevel += f
              case _ => sys.error(s"Not implemented yet: $getClass")
            }
        }
        if (topLevel.nonEmpty) {
          val defs = topLevel.toList.map(x => GenFun(x)(context).tree)
          val pos = Position(Position.SourceFile(file.getName), 0, 0)

          val name = JvmFileClassUtil.getFileClassInfoNoResolve(file).getFileClassFqName.asString()
          val encodedName = NameEncoder.encodeClassName(name, "")
          val cls = ClassDef(Ident(encodedName)(pos), ClassKind.Class, None, List(), None, defs)(OptimizerHints.empty)(pos)
          SJSIRCodegen.genIRFile(output, name, cls)
        }
      } catch {
        case e: TranslationRuntimeException => throw e
        case e: RuntimeException => throw new TranslationRuntimeException(file, e)
        case e: AssertionError => throw new TranslationRuntimeException(file, e)
      }
    }
  }

}
