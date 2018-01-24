package ch.epfl.k2sjsir

import java.util

import org.jetbrains.kotlin.diagnostics.DiagnosticUtils.hasError
import org.jetbrains.kotlin.js.analyze.TopDownAnalyzerFacadeForJS
import org.jetbrains.kotlin.js.analyzer.JsAnalysisResult
import org.jetbrains.kotlin.js.backend.ast.{JsImportedModule, JsName}
import org.jetbrains.kotlin.js.config.JsConfig.Reporter
import org.jetbrains.kotlin.js.config.{JSConfigurationKeys, JsConfig}
import org.jetbrains.kotlin.js.coroutine.CoroutineTransformer
import org.jetbrains.kotlin.js.facade.exceptions.TranslationException
import org.jetbrains.kotlin.js.facade.{MainCallParameters, TranslationResult, TranslationUnit}
import org.jetbrains.kotlin.js.inline.JsInliner
import org.jetbrains.kotlin.js.inline.clean.{RemoveUnusedImportsKt, ResolveTemporaryNamesKt}
import org.jetbrains.kotlin.js.sourceMap.SourceFilePathResolver
import org.jetbrains.kotlin.js.translate.utils.ExpandIsCallsKt.expandIsCalls
import org.jetbrains.kotlin.progress.ProgressIndicatorAndCompilationCanceledStatus
import org.jetbrains.kotlin.psi.KtFile

import scala.collection.JavaConverters._

final class K2SJSIRTranslator(val config: JsConfig) {

  val incrementalResults = config.getConfiguration.get(
    JSConfigurationKeys.INCREMENTAL_RESULTS_CONSUMER)

  @throws[TranslationException]
  def translate(reporter: JsConfig.Reporter,
                files: List[KtFile],
                parameters: MainCallParameters): TranslationResult =
    translate(reporter, files, parameters, ar = null)

  @throws[TranslationException]
  def translate(reporter: JsConfig.Reporter,
                sourceFiles: List[KtFile],
                parameters: MainCallParameters,
                ar: JsAnalysisResult): TranslationResult = {

    val units = sourceFiles.map(new TranslationUnit.SourceFile(_))

    val files = new util.ArrayList[KtFile](sourceFiles.asJava)

    val analysisResult =
      if (ar == null)
        TopDownAnalyzerFacadeForJS.analyzeFiles(files, config)
      else
        ar

    ProgressIndicatorAndCompilationCanceledStatus.checkCanceled()

    val bindingTrace = analysisResult.getBindingTrace
    TopDownAnalyzerFacadeForJS.checkForErrors(files,
                                              bindingTrace.getBindingContext)

    val moduleDescriptor = analysisResult.getModuleDescriptor

    val diagnostics = bindingTrace.getBindingContext.getDiagnostics

    val pathResolver = SourceFilePathResolver.create(config)
    val translationResult = Translation.generateAst(bindingTrace, files, parameters, moduleDescriptor, config, pathResolver)
    ProgressIndicatorAndCompilationCanceledStatus.checkCanceled()
    if (hasError(diagnostics))
      return new TranslationResult.Fail(diagnostics)

    val newFragments = translationResult.getNewFragments
    val allFragments = translationResult.getFragments

    JsInliner.process(reporter,
      config,
      analysisResult.getBindingTrace,
      translationResult.getInnerModuleName,
      allFragments,
      newFragments,
      translationResult.getImportStatements
    )
    /*
    ResolveTemporaryNamesKt.resolveTemporaryNames(program)

    ProgressIndicatorAndCompilationCanceledStatus.checkCanceled()

    if (hasError(diagnostics))
      return new TranslationResult.Fail(diagnostics)

    val coroutineTransformer = new CoroutineTransformer(program)
    coroutineTransformer.accept(program)

    RemoveUnusedImportsKt.removeUnusedImports(program)

    ProgressIndicatorAndCompilationCanceledStatus.checkCanceled()

    if (hasError(diagnostics))
      return new TranslationResult.Fail(diagnostics)

    expandIsCalls(program, context)
    */

    ProgressIndicatorAndCompilationCanceledStatus.checkCanceled()

    val importedModules = new util.ArrayList[String]

    for {
      module <- translationResult.getImportedModuleList.asScala
    } yield importedModules.add(module.getExternalName)

    new TranslationResult.Success(config,
                                  files,
                                  translationResult.getProgram,
                                  diagnostics,
                                  importedModules,
                                  moduleDescriptor,
                                  bindingTrace.getBindingContext)
  }
}
