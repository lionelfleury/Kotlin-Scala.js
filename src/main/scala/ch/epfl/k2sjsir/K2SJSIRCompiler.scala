package ch.epfl.k2sjsir

/*
 * Copyright 2010-2015 JetBrains s.r.o.
 * Adapted 2017 by Lionel Fleury and Guillaume Tournigand
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import java.io.File
import java.{lang => jl, util => ju}

import com.google.common.base.Joiner
import com.intellij.openapi.Disposable
import com.intellij.openapi.util.io.FileUtil
import com.intellij.util.containers.ContainerUtil
import com.intellij.util.{Function, SmartList}
import org.jetbrains.kotlin.analyzer.AnalysisResult
import org.jetbrains.kotlin.cli.common.CLICompiler.doMain
import org.jetbrains.kotlin.cli.common.ExitCode.{COMPILATION_ERROR, OK}
import org.jetbrains.kotlin.cli.common.UtilsKt.checkKotlinPackageUsage
import org.jetbrains.kotlin.cli.common.arguments.K2JsArgumentConstants
import org.jetbrains.kotlin.cli.common.messages.CompilerMessageLocation.NO_LOCATION
import org.jetbrains.kotlin.cli.common.messages._
import org.jetbrains.kotlin.cli.common.{CLICompiler, CLIConfigurationKeys, ExitCode}
import org.jetbrains.kotlin.cli.jvm.compiler.{EnvironmentConfigFiles, KotlinCoreEnvironment}
import org.jetbrains.kotlin.config._
import org.jetbrains.kotlin.js.analyze.TopDownAnalyzerFacadeForJS
import org.jetbrains.kotlin.js.analyzer.JsAnalysisResult
import org.jetbrains.kotlin.js.config.{EcmaVersion, JSConfigurationKeys, JsConfig, LibrarySourcesConfig}
import org.jetbrains.kotlin.js.facade.{MainCallParameters, TranslationResult}
import org.jetbrains.kotlin.progress.ProgressIndicatorAndCompilationCanceledStatus
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.serialization.js.ModuleKind
import org.jetbrains.kotlin.utils.{ExceptionUtilsKt, KotlinPaths, KotlinPathsFromHomeDir, PathUtil}

import scala.collection.JavaConverters._

object K2SJSIRCompiler {

  def main(args: Array[String]): Unit = doMain(new K2SJSIRCompiler, args)

  private def reportCompiledSourcesList(messageCollector: MessageCollector, sourceFiles: ju.List[KtFile]) = {
    val fileNames = ContainerUtil.map(sourceFiles, new Function[KtFile, String]() {
      override def fun(file: KtFile): String = {
        assert(file != null)
        val virtualFile = file.getVirtualFile
        if (virtualFile != null) return FileUtil.toSystemDependentName(virtualFile.getPath)
        file.getName + "(no virtual file)"
      }
    })
    messageCollector.report(CompilerMessageSeverity.LOGGING, "Compiling source files: " + Joiner.on(", ").join(fileNames), CompilerMessageLocation.NO_LOCATION)
  }

  private def analyzeAndReportErrors(messageCollector: MessageCollector, sources: ju.List[KtFile], config: JsConfig) = {
    val analyzerWithCompilerReport = new AnalyzerWithCompilerReport(messageCollector)
    analyzerWithCompilerReport.analyzeAndReport(sources, new AnalyzerWithCompilerReport.Analyzer() {
      override def analyze: AnalysisResult = TopDownAnalyzerFacadeForJS.analyzeFiles(sources, config)
      override def reportEnvironmentErrors(): Unit = {}
    })
    analyzerWithCompilerReport
  }

  private def createMainCallParameters(main: String) =
    if (K2JsArgumentConstants.NO_CALL == main) MainCallParameters.noCall
    else MainCallParameters.mainWithoutArguments

}

class K2SJSIRCompiler extends CLICompiler[K2SJSIRCompilerArguments] {

  override protected def createArguments = new K2SJSIRCompilerArguments()

  override protected def doExecute(
    arguments: K2SJSIRCompilerArguments,
    configuration: CompilerConfiguration,
    rootDisposable: Disposable
  ): ExitCode = {
    val messageCollector = configuration.getNotNull(CLIConfigurationKeys.MESSAGE_COLLECTOR_KEY)
    if (arguments.freeArgs.isEmpty) {
      if (arguments.version) return OK
      messageCollector.report(CompilerMessageSeverity.ERROR, "Specify at least one source file or directory", NO_LOCATION)
      return COMPILATION_ERROR
    }
    ContentRootsKt.addKotlinSourceRoots(configuration, arguments.freeArgs)
    val paths: KotlinPaths =
      if (arguments.kotlinHome != null) new KotlinPathsFromHomeDir(new File(arguments.kotlinHome))
      else PathUtil.getKotlinPathsForCompiler
    messageCollector.report(CompilerMessageSeverity.LOGGING, "Using Kotlin home directory " + paths.getHomePath, CompilerMessageLocation.NO_LOCATION)

    //FIXME: create maybe a js file from Kotlin
    configuration.put(CommonConfigurationKeys.MODULE_NAME, arguments.destination)

    val libraries = new SmartList[String]
    if (!arguments.noStdlib)
      libraries.add(0, paths.getJsStdLibJarPath.getAbsolutePath)

//    if (arguments.libraries != null)
//      ContainerUtil.addAllNotNull(libraries, arguments.libraries.split(File.pathSeparator).toList.asJavaCollection)

    configuration.put(JSConfigurationKeys.LIBRARIES, libraries)
    val environmentForJS = KotlinCoreEnvironment.createForProduction(rootDisposable, configuration, EnvironmentConfigFiles.JS_CONFIG_FILES)
    val project = environmentForJS.getProject
    val sourcesFiles = environmentForJS.getSourceFiles
    environmentForJS.getConfiguration.put[jl.Boolean](CLIConfigurationKeys.ALLOW_KOTLIN_PACKAGE, arguments.allowKotlinPackage)
    if (!checkKotlinPackageUsage(environmentForJS, sourcesFiles)) return ExitCode.COMPILATION_ERROR
    if (arguments.destination == null) {
      messageCollector.report(CompilerMessageSeverity.ERROR, "Specify output directory via -d", CompilerMessageLocation.NO_LOCATION)
      return ExitCode.COMPILATION_ERROR
    }
    if (messageCollector.hasErrors) return ExitCode.COMPILATION_ERROR
    if (sourcesFiles.isEmpty) {
      messageCollector.report(CompilerMessageSeverity.ERROR, "No source files", CompilerMessageLocation.NO_LOCATION)
      return COMPILATION_ERROR
    }
    if (arguments.verbose) K2SJSIRCompiler.reportCompiledSourcesList(messageCollector, sourcesFiles)
    val destination = arguments.destination
    if (destination != null) {
      if (destination.endsWith(".jar")) configuration.put(JVMConfigurationKeys.OUTPUT_JAR, new File(destination))
      else configuration.put(JVMConfigurationKeys.OUTPUT_DIRECTORY, new File(destination))
    }
    val config = new LibrarySourcesConfig(project, configuration)
    if (config.checkLibFilesAndReportErrors(new JsConfig.Reporter() {
      override def error(message: String): Unit =
        messageCollector.report(CompilerMessageSeverity.ERROR, message, CompilerMessageLocation.NO_LOCATION)
      override def warning(message: String): Unit =
        messageCollector.report(CompilerMessageSeverity.STRONG_WARNING, message, CompilerMessageLocation.NO_LOCATION)
    })) return COMPILATION_ERROR
    val analyzerWithCompilerReport = K2SJSIRCompiler.analyzeAndReportErrors(messageCollector, sourcesFiles, config)
    if (analyzerWithCompilerReport.hasErrors) return COMPILATION_ERROR
    ProgressIndicatorAndCompilationCanceledStatus.checkCanceled()
    val analysisResult = analyzerWithCompilerReport.getAnalysisResult
    assert(analysisResult.isInstanceOf[JsAnalysisResult], "analysisResult should be instance of JsAnalysisResult, but " + analysisResult)
    val jsAnalysisResult = analysisResult.asInstanceOf[JsAnalysisResult]

    val mainCallParameters = K2SJSIRCompiler.createMainCallParameters(arguments.main)
    var translationResult: TranslationResult = null
    val translator = new K2SJSIRTranslator(config)
    try {  //noinspection unchecked
      translationResult = translator.translate(sourcesFiles, mainCallParameters, jsAnalysisResult)
    } catch {
      case e: Exception => throw ExceptionUtilsKt.rethrow(e)
    }
    ProgressIndicatorAndCompilationCanceledStatus.checkCanceled()
//    (new AnalyzerWithCompilerReport.Companion).reportDiagnostics(translationResult.getDiagnostics, messageCollector)
    if (!translationResult.isInstanceOf[TranslationResult.Success]) return ExitCode.COMPILATION_ERROR
    ProgressIndicatorAndCompilationCanceledStatus.checkCanceled()
    OK
  }

  override def setupPlatformSpecificArgumentsAndServices(configuration: CompilerConfiguration, arguments: K2SJSIRCompilerArguments, services: Services): Unit = {
    configuration.put[jl.Boolean](JSConfigurationKeys.SOURCE_MAP, true)
    configuration.put(JSConfigurationKeys.TARGET, EcmaVersion.defaultVersion)
    configuration.put(JSConfigurationKeys.MODULE_KIND, ModuleKind.PLAIN)
  }

}
