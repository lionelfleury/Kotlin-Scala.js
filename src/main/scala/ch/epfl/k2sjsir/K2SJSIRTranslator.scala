package ch.epfl.k2sjsir

import java.util

import org.jetbrains.kotlin.diagnostics.DiagnosticUtils.hasError
import org.jetbrains.kotlin.js.analyze.TopDownAnalyzerFacadeForJS
import org.jetbrains.kotlin.js.analyzer.JsAnalysisResult
import org.jetbrains.kotlin.js.config.JsConfig
import org.jetbrains.kotlin.js.coroutine.CoroutineTransformer
import org.jetbrains.kotlin.js.facade.exceptions.TranslationException
import org.jetbrains.kotlin.js.facade.{MainCallParameters, TranslationResult}
import org.jetbrains.kotlin.js.inline.JsInliner
import org.jetbrains.kotlin.js.inline.clean.{RemoveUnusedImportsKt, ResolveTemporaryNamesKt}
import org.jetbrains.kotlin.js.translate.utils.ExpandIsCallsKt.expandIsCalls
import org.jetbrains.kotlin.progress.ProgressIndicatorAndCompilationCanceledStatus
import org.jetbrains.kotlin.psi.KtFile

import scala.collection.JavaConverters._

final class K2SJSIRTranslator(val config: JsConfig) {
  @throws[TranslationException]
  def translate(files: util.List[KtFile], mainCallParameters: MainCallParameters): TranslationResult =
    translate(files, mainCallParameters, null)

  @throws[TranslationException]
  def translate(files: util.List[KtFile], mainCallParameters: MainCallParameters, ar: JsAnalysisResult): TranslationResult = {
    var analysisResult = ar
    if (analysisResult == null) {
      analysisResult = TopDownAnalyzerFacadeForJS.analyzeFiles(files, config)
      ProgressIndicatorAndCompilationCanceledStatus.checkCanceled()
    }
    val bindingTrace = analysisResult.getBindingTrace
    TopDownAnalyzerFacadeForJS.checkForErrors(JsConfig.withJsLibAdded(files, config), bindingTrace.getBindingContext)
    val moduleDescriptor = analysisResult.getModuleDescriptor
    val diagnostics = bindingTrace.getBindingContext.getDiagnostics
    val context = Translation.generateAst(bindingTrace, files, mainCallParameters, moduleDescriptor, config)
    ProgressIndicatorAndCompilationCanceledStatus.checkCanceled()
    if (hasError(diagnostics)) return new TranslationResult.Fail(diagnostics)
    val program = JsInliner.process(context)
    ResolveTemporaryNamesKt.resolveTemporaryNames(program)
    ProgressIndicatorAndCompilationCanceledStatus.checkCanceled()
    if (hasError(diagnostics)) return new TranslationResult.Fail(diagnostics)
    val coroutineTransformer = new CoroutineTransformer(program)
    coroutineTransformer.accept(program)
    RemoveUnusedImportsKt.removeUnusedImports(program)
    ProgressIndicatorAndCompilationCanceledStatus.checkCanceled()
    if (hasError(diagnostics)) return new TranslationResult.Fail(diagnostics)
    expandIsCalls(program, context)
    ProgressIndicatorAndCompilationCanceledStatus.checkCanceled()
    val importedModules = new util.ArrayList[String]

    for (module <- context.getImportedModules.asScala) {
      importedModules.add(module.getExternalName)
    }
    new TranslationResult.Success(config, files, program, diagnostics, importedModules, moduleDescriptor, bindingTrace.getBindingContext)
  }
}
