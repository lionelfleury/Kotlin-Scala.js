package ch.epfl.k2sjsir

import java.{util => ju}

import ch.epfl.k2sjsir.translate.GenClass
import org.jetbrains.kotlin.js.facade.exceptions.TranslationRuntimeException
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.declaration.FileDeclarationVisitor
import org.jetbrains.kotlin.js.translate.general.AbstractTranslator
import org.jetbrains.kotlin.js.translate.utils.{AnnotationsUtils, BindingUtils}
import org.jetbrains.kotlin.psi.{KtClassOrObject, KtFile}

import scala.collection.JavaConversions._


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
    for (file <- files) {
      try {
        for (declaration <- file.getDeclarations) {
          if (!AnnotationsUtils.isPredefinedObject(BindingUtils.getDescriptorForElement(bindingContext, declaration)))
            declaration match {
              case d: KtClassOrObject => GenClass(d, context).tree
            }
        }
      } catch {
        case e: TranslationRuntimeException => throw e
        case e: RuntimeException => throw new TranslationRuntimeException(file, e)
        case e: AssertionError => throw new TranslationRuntimeException(file, e)
      }
    }
  }

}
