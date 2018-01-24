package ch.epfl.k2sjsir

/*
 * Copyright 2010-2015 JetBrains s.r.o.
 * Adapted 2017 by Lionel Fleury and Guillaume Tournigand
 * Adapted 2017 by Florian Alonso
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
import java.net.URLClassLoader
import java.{lang => jl, util => ju}

import org.jetbrains.kotlin.com.intellij.openapi.Disposable
import org.jetbrains.kotlin.com.intellij.openapi.util.io.FileUtil
import org.jetbrains.kotlin.com.intellij.util.containers.ContainerUtil
import org.jetbrains.kotlin.com.intellij.util.{Function, SmartList}
import kotlin.jvm.functions.Function1
import org.jetbrains.kotlin.cli.common.CLITool.doMain
import org.jetbrains.kotlin.cli.common.ExitCode.{COMPILATION_ERROR, OK}
import org.jetbrains.kotlin.cli.common.UtilsKt.checkKotlinPackageUsage
import org.jetbrains.kotlin.cli.common.arguments.K2JsArgumentConstants
import org.jetbrains.kotlin.cli.common.messages._
import org.jetbrains.kotlin.cli.common.{CLICompiler, CLIConfigurationKeys, ExitCode}
import org.jetbrains.kotlin.cli.jvm.compiler.{EnvironmentConfigFiles, KotlinCoreEnvironment}
import org.jetbrains.kotlin.cli.jvm.plugins.PluginCliParser
import org.jetbrains.kotlin.config._
import org.jetbrains.kotlin.js.analyze.TopDownAnalyzerFacadeForJS
import org.jetbrains.kotlin.js.analyzer.JsAnalysisResult
import org.jetbrains.kotlin.js.config.{EcmaVersion, JSConfigurationKeys, JsConfig}
import org.jetbrains.kotlin.js.facade.{MainCallParameters, TranslationResult}
import org.jetbrains.kotlin.progress.ProgressIndicatorAndCompilationCanceledStatus
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.serialization.js.ModuleKind
import org.jetbrains.kotlin.utils._

import scala.collection.JavaConverters._

object K2SJSIRCompiler {

  def main(args: Array[String]): Unit = doMain(new K2SJSIRCompiler, args)

  private def reportCompiledSourcesList(messageCollector: MessageCollector,
                                        sourceFiles: ju.List[KtFile]) = {
    val fileNames = ContainerUtil.map(
      sourceFiles,
      new Function[KtFile, String]() {
        override def fun(file: KtFile): String = {
          assert(file != null)
          val virtualFile = file.getVirtualFile
          if (virtualFile != null)
            return FileUtil.toSystemDependentName(virtualFile.getPath)
          file.getName + "(no virtual file)"
        }
      }
    )
    messageCollector.report(
      CompilerMessageSeverity.LOGGING,
      "Compiling source files: " + StringsKt.join(fileNames, ", "),
      null)
  }

  private def analyzeAndReportErrors(messageCollector: MessageCollector,
                                     sources: ju.List[KtFile],
                                     config: JsConfig) = {
    val analyzerWithCompilerReport = new AnalyzerWithCompilerReport(
      messageCollector)

    analyzerWithCompilerReport.analyzeAndReport(
      sources,
      () => TopDownAnalyzerFacadeForJS.analyzeFiles(sources, config)
    )
    analyzerWithCompilerReport
  }

  private def createMainCallParameters(main: String) =
    if (K2JsArgumentConstants.NO_CALL == main)
      MainCallParameters.noCall
    else
      MainCallParameters.mainWithoutArguments

  private def configureLibraries(
                                  arguments: K2SJSIRCompilerArguments,
                                  paths: KotlinPaths,
                                  messageCollector: MessageCollector): java.util.List[String] = {
    val libraries: java.util.List[String] = new SmartList[String]

    if (!arguments.noStdlib) {
      val func = new Function1[KotlinPaths, File] {
        override def invoke(x: KotlinPaths): File = {
          x.getJsStdLibJarPath
        }
      }

      val stdLibJar = CLICompiler.getLibraryFromHome(paths,
                                                     func,
                                                     PathUtil.JS_LIB_JAR_NAME,
                                                     messageCollector,
                                                     "'-no-stdlib'")

      if (stdLibJar != null)
        libraries.add(0, stdLibJar.getAbsolutePath)
    }

    if (arguments.libraries != null)
      ContainerUtil.addAllNotNull(
        libraries,
        arguments.libraries.split(File.pathSeparator).toList: _*)

    libraries
  }
}

class K2SJSIRCompiler extends CLICompiler[K2SJSIRCompilerArguments] {

  override def createArguments = new K2SJSIRCompilerArguments()

  override protected def doExecute(
                                    arguments: K2SJSIRCompilerArguments,
                                    configuration: CompilerConfiguration,
                                    rootDisposable: Disposable,
                                    paths: KotlinPaths
  ): ExitCode = {

    val messageCollector = configuration.getNotNull(CLIConfigurationKeys.MESSAGE_COLLECTOR_KEY)

    if (arguments.getFreeArgs.isEmpty) {
      if (arguments.getVersion)
        return OK

      messageCollector.report(CompilerMessageSeverity.ERROR,
                              "Specify at least one source file or directory",
                              null)
      return COMPILATION_ERROR
    }

    val plugLoadResult =
      PluginCliParser.loadPluginsSafe(arguments, configuration)

    if (plugLoadResult != ExitCode.OK)
      return plugLoadResult


    val paths: KotlinPaths =
      if (arguments.getKotlinHome != null)
        new KotlinPathsFromHomeDir(new File(arguments.getKotlinHome))
      else PathUtil.getKotlinPathsForCompiler

    messageCollector.report(CompilerMessageSeverity.LOGGING,
                            "Using Kotlin home directory " + paths.getHomePath,
                            null)

    val libraries = K2SJSIRCompiler.configureLibraries(arguments, paths, messageCollector)
    configuration.put(
      JSConfigurationKeys.LIBRARIES,
      libraries)

    ContentRootsKt.addKotlinSourceRoots(configuration, arguments.getFreeArgs)
    val environmentForJS = KotlinCoreEnvironment.createForProduction(
      rootDisposable,
      configuration,
      EnvironmentConfigFiles.JS_CONFIG_FILES)

    val project = environmentForJS.getProject
    val sourcesFiles = environmentForJS.getSourceFiles

    environmentForJS.getConfiguration.put[jl.Boolean](
      CLIConfigurationKeys.ALLOW_KOTLIN_PACKAGE,
      arguments.getAllowKotlinPackage)

    if (!checkKotlinPackageUsage(environmentForJS, sourcesFiles))
      return ExitCode.COMPILATION_ERROR

    if (arguments.destination == null) {
      messageCollector.report(CompilerMessageSeverity.ERROR,
                              "Specify output directory via -d",
                              null)
      return ExitCode.COMPILATION_ERROR
    }

    if (messageCollector.hasErrors)
      return ExitCode.COMPILATION_ERROR

    if (sourcesFiles.isEmpty) {
      messageCollector.report(CompilerMessageSeverity.ERROR,
                              "No source files",
                              null)
      return COMPILATION_ERROR
    }

    if (arguments.getVerbose)
      K2SJSIRCompiler.reportCompiledSourcesList(messageCollector, sourcesFiles)

    configuration.put(CommonConfigurationKeys.MODULE_NAME,
                      arguments.destination)

    if (arguments.destination != null) {
      val destination = new File(arguments.destination)

      if (arguments.destination.endsWith(".jar"))
        configuration.put(JVMConfigurationKeys.OUTPUT_JAR, destination)
      else
        configuration.put(JVMConfigurationKeys.OUTPUT_DIRECTORY, destination)
    }

    val config = new JsConfig(project, configuration)
    val reporter = new JsConfig.Reporter() {
      override def error(message: String): Unit =
        messageCollector.report(CompilerMessageSeverity.ERROR, message, null)

      override def warning(message: String): Unit =
        messageCollector.report(CompilerMessageSeverity.STRONG_WARNING,
                                message,
                                null)
    }

    if (config.checkLibFilesAndReportErrors(reporter))
      return COMPILATION_ERROR

    val analyzerWithCompilerReport = K2SJSIRCompiler.analyzeAndReportErrors(
      messageCollector,
      sourcesFiles,
      config)
    if (analyzerWithCompilerReport.hasErrors) return COMPILATION_ERROR

    ProgressIndicatorAndCompilationCanceledStatus.checkCanceled()

    val analysisResult = analyzerWithCompilerReport.getAnalysisResult
    assert(
      analysisResult.isInstanceOf[JsAnalysisResult],
      "analysisResult should be instance of JsAnalysisResult, but " + analysisResult)
    val jsAnalysisResult = analysisResult.asInstanceOf[JsAnalysisResult]

    val mainCallParameters =
      K2SJSIRCompiler.createMainCallParameters(arguments.main)

    val translator = new K2SJSIRTranslator(config)
    val translationResult: TranslationResult =
      try { //noinspection unchecked
        translator.translate(reporter,
                             sourcesFiles.asScala.toList,
                             mainCallParameters,
                             jsAnalysisResult)
      } catch {
        case e: Exception => throw ExceptionUtilsKt.rethrow(e)
      }

    ProgressIndicatorAndCompilationCanceledStatus.checkCanceled()

    K2SJSIRCompilerArguments.dirtyCallToCompanion(translationResult.getDiagnostics, messageCollector)

    if (!translationResult.isInstanceOf[TranslationResult.Success])
      return ExitCode.COMPILATION_ERROR

    ProgressIndicatorAndCompilationCanceledStatus.checkCanceled()
    OK
  }

  override def setupPlatformSpecificArgumentsAndServices(
                                                          configuration: CompilerConfiguration,
                                                          arguments: K2SJSIRCompilerArguments,
                                                          services: Services): Unit = {
    configuration.put[jl.Boolean](JSConfigurationKeys.SOURCE_MAP, true)
    configuration.put(JSConfigurationKeys.TARGET, EcmaVersion.defaultVersion)
    configuration.put(JSConfigurationKeys.MODULE_KIND, ModuleKind.PLAIN)
  }

  override def executableScriptFileName() = "kotin-to-scalajsc"
}
