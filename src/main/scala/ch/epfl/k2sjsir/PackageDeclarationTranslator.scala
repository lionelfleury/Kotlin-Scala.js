package ch.epfl.k2sjsir

import java.{util => ju}

import ch.epfl.k2sjsir.lower.SJSIRLower
import ch.epfl.k2sjsir.translate._
import ch.epfl.k2sjsir.utils.{NameEncoder, Utils}
import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.backend.jvm.lower.InterfaceLowering
import org.jetbrains.kotlin.cli.common.CLIConfigurationKeys
import org.jetbrains.kotlin.cli.common.messages.CompilerMessageSeverity
import org.jetbrains.kotlin.config.CommonConfigurationKeys
import org.jetbrains.kotlin.diagnostics.DiagnosticUtils
import org.jetbrains.kotlin.fileClasses.JvmFileClassUtil
import org.jetbrains.kotlin.js.facade.exceptions.TranslationRuntimeException
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.general.AbstractTranslator
import org.jetbrains.kotlin.js.translate.utils.BindingUtils._
import org.jetbrains.kotlin.js.translate.utils.{AnnotationsUtils, BindingUtils}
import org.jetbrains.kotlin.psi._
import org.jetbrains.kotlin.descriptors.{ClassKind => KtClassKind}
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types._
import org.scalajs.core.ir.{ClassKind, Definitions, Position}

import scala.collection.JavaConversions._
import scala.collection.immutable.{List, Nil}
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
    val messageCollector = context.getConfig.getConfiguration.getNotNull(CLIConfigurationKeys.MESSAGE_COLLECTOR_KEY)

    for (file <- files) {
      try {
        val topLevelFunctions = new mutable.MutableList[KtNamedFunction]()
        val topLevelValues = new mutable.MutableList[KtProperty]()
        val declarations = new SJSIRLower().lower(file)

        for (declaration <- declarations) {
          val predefinedObject = AnnotationsUtils.isPredefinedObject(BindingUtils.getDescriptorForElement(bindingContext, declaration))

          declaration match {
            case d: KtClassOrObject if !predefinedObject=>
              val cd = getClassDescriptor(context.bindingContext(), d)
              if (cd.getKind == KtClassKind.ANNOTATION_CLASS) {
                messageCollector.report(CompilerMessageSeverity.WARNING, s"SKIPPED ANNOTATION CLASS ${cd.toJsName}", null)
              } else {
                val genClass = GenClass(d)(context)

                val tree = genClass.tree
                val treeDefaultImpls: Option[ClassDef] = genClass.treeDefaultImpls
                val treeEnumCompanion: Option[ClassDef] = genClass.treeEnumCompanion


                SJSIRCodegen.genIRFile(output, cd, tree)
                treeDefaultImpls.foreach(SJSIRCodegen.genIRDefaultImpls(output, cd, _))
                treeEnumCompanion.foreach(SJSIRCodegen.genIREnumCompanion(output, cd, _))
              }
            case d: KtClassOrObject =>
              val tree = GenExternalClass(d)(context).tree
              val cd = getClassDescriptor(context.bindingContext(), d)
              SJSIRCodegen.genIRFile(output, cd, tree)

            case f: KtNamedFunction =>
              topLevelFunctions += f

            case p: KtProperty =>
              topLevelValues += p

            case t => sys.error(s"Not implemented yet: ${t.getClass}")
          }
        }

        if (topLevelFunctions.nonEmpty || topLevelValues.nonEmpty) {
          val clsTpe = file.toJsClassType
          val encodedName = clsTpe.className
          val className = file.getFileClassName

          implicit val pos = Position(Position.SourceFile(file.getName), 0, 0)

          val defs = topLevelFunctions.toList.map(x => GenFun(x)(context).tree)
          val vals = topLevelValues.flatMap(x => GenProperty(x)(context).withGetterAndSetter)
          val receiver = Utils.genThisFromContext(clsTpe)

          val objctSuperCall = ApplyStatically(receiver, ClassType(Definitions.ObjectClass), Ident("init___"), Nil)(NoType)
          // TODO: Initialize top level variables and values (these should be static?)
          val ctorBody = Block(objctSuperCall)
          val ctor = MethodDef(false, Ident("init___"), Nil, NoType,
            Some(ctorBody))(OptimizerHints.empty, None)

          val ctorAndDefs = ctor :: (defs ++ vals)

          val hasMain = defs.exists {
            case MethodDef(_, Ident("main__AT__V", _), _, _, _) => true
            case _ => false
          }

          val cls = ClassDef(
              Ident(encodedName)(pos),
              ClassKind.ModuleClass,
              None,
              Some(Ident(Definitions.ObjectClass)(pos)),
              List(),
              None,
              None,
              ctorAndDefs,
              Nil)(OptimizerHints.empty)(pos)

          val name = encodedName.drop(1).replace("_", "/")
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
