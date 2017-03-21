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

import com.intellij.openapi.Disposable
import org.jetbrains.kotlin.cli.common.UtilsKt.checkKotlinPackageUsage
import org.jetbrains.kotlin.cli.common.arguments.K2JVMCompilerArguments
import org.jetbrains.kotlin.cli.common.messages._
import org.jetbrains.kotlin.cli.common.{CLICompiler, CLIConfigurationKeys, ExitCode}
import org.jetbrains.kotlin.cli.jvm.PluginCliParser
import org.jetbrains.kotlin.cli.jvm.compiler.{EnvironmentConfigFiles, KotlinCoreEnvironment, KotlinToJVMBytecodeCompiler}
import org.jetbrains.kotlin.cli.jvm.config.{JavaSourceRoot, JvmClasspathRoot}
import org.jetbrains.kotlin.codegen.CompilationException
import org.jetbrains.kotlin.config._
import org.jetbrains.kotlin.load.java.JvmAbi
import org.jetbrains.kotlin.load.kotlin.incremental.components.IncrementalCompilationComponents
import org.jetbrains.kotlin.utils.{KotlinPaths, KotlinPathsFromHomeDir, PathUtil}

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

class K2SJSIRCompiler extends CLICompiler[K2JVMCompilerArguments]() {

  import K2SJSIRCompiler._

  override def doExecute(arguments: K2JVMCompilerArguments, configuration: CompilerConfiguration, rootDisposable: Disposable): ExitCode = {
    val messageCollector = configuration.getNotNull(CLIConfigurationKeys.MESSAGE_COLLECTOR_KEY)
    val paths =
      if (arguments.kotlinHome != null) new KotlinPathsFromHomeDir(new File(arguments.kotlinHome))
      else PathUtil.getKotlinPathsForCompiler
    messageCollector.report(CompilerMessageSeverity.LOGGING, "Using Kotlin home directory " + paths.getHomePath, CompilerMessageLocation.NO_LOCATION)
    val it = setupJdkClasspathRoots(arguments, configuration, messageCollector)
    if (it != ExitCode.OK) return it
    try {
      PluginCliParser.loadPlugins(arguments, configuration)
    } catch {
      case t: Throwable =>
        MessageCollectorUtil.reportException(messageCollector, t)
        return ExitCode.INTERNAL_ERROR
    }
    if (arguments.module == null) {
      for (arg <- arguments.freeArgs.asScala) {
        configuration.add(JVMConfigurationKeys.CONTENT_ROOTS, new KotlinSourceRoot(arg))
        val file = new File(arg)
        if (file.isDirectory) {
          configuration.add(JVMConfigurationKeys.CONTENT_ROOTS, new JavaSourceRoot(file, ""))
        }
      }
    }
    val classpath = getClasspath(paths, arguments)
    for (file <- classpath.asScala)
      configuration.add(JVMConfigurationKeys.CONTENT_ROOTS, new JvmClasspathRoot(file))
    val module = if (arguments.moduleName == null) JvmAbi.DEFAULT_MODULE_NAME else arguments.moduleName
    configuration.put(CommonConfigurationKeys.MODULE_NAME, module)
    if (arguments.module == null && arguments.freeArgs.isEmpty && !arguments.version) {
      println("No source file given")
      return ExitCode.OK
    }
    val friendPaths = arguments.friendPaths
    if (friendPaths != null && friendPaths.nonEmpty) {
      configuration.put[ju.List[String]](JVMConfigurationKeys.FRIEND_PATHS, friendPaths.toList.asJava)
    }
    configuration.put[jl.Boolean](JVMConfigurationKeys.PARAMETERS_METADATA, arguments.javaParameters)
    putAdvancedOptions(configuration, arguments)
    messageCollector.report(CompilerMessageSeverity.LOGGING, "Configuring the compilation environment", CompilerMessageLocation.NO_LOCATION)
    try {
      val destination = arguments.destination
      if (destination != null) {
        if (destination.endsWith(".jar")) configuration.put(JVMConfigurationKeys.OUTPUT_JAR, new File(destination))
        else configuration.put(JVMConfigurationKeys.OUTPUT_DIRECTORY, new File(destination))
      }
      val environment = createCoreEnvironment(rootDisposable, configuration)
      if (environment == null) return ExitCode.COMPILATION_ERROR
      if (environment.getSourceFiles.isEmpty) {
        if (arguments.version) {
          return ExitCode.OK
        }
        messageCollector.report(CompilerMessageSeverity.ERROR, "No source files", CompilerMessageLocation.NO_LOCATION)
        return ExitCode.COMPILATION_ERROR
      }
      if (!checkKotlinPackageUsage(environment, environment.getSourceFiles)) return ExitCode.INTERNAL_ERROR
      val generationState = KotlinToJVMBytecodeCompiler.INSTANCE.analyzeAndGenerate(environment)
      // Call to our Backend...
      Backend.doGenerateFiles(generationState)
      ExitCode.OK
    } catch {
      case e: CompilationException =>
        messageCollector.report(CompilerMessageSeverity.EXCEPTION, OutputMessageUtil.renderException(e), MessageUtil.psiElementToMessageLocation(e.getElement))
        ExitCode.INTERNAL_ERROR
    }
  }

  override def setupPlatformSpecificArgumentsAndServices(configuration: CompilerConfiguration, a: K2JVMCompilerArguments, services: Services): Unit = {
    if (IncrementalCompilation.isEnabled) {
      val components = services.get(classOf[IncrementalCompilationComponents])
      if (components != null) {
        configuration.put(JVMConfigurationKeys.INCREMENTAL_COMPILATION_COMPONENTS, components)
      }
    }
  }

  override def createArguments(): K2JVMCompilerArguments = new K2JVMCompilerArguments()

}

object K2SJSIRCompiler {

  def main(args: Array[String]): Unit = CLICompiler.doMain(new K2SJSIRCompiler(), args)

  private def putAdvancedOptions(configuration: CompilerConfiguration, arguments: K2JVMCompilerArguments) {
    configuration.put[jl.Boolean](JVMConfigurationKeys.DISABLE_CALL_ASSERTIONS, arguments.noCallAssertions)
    configuration.put[jl.Boolean](JVMConfigurationKeys.DISABLE_PARAM_ASSERTIONS, arguments.noParamAssertions)
    configuration.put[jl.Boolean](JVMConfigurationKeys.DISABLE_OPTIMIZATION, arguments.noOptimize)
    configuration.put[jl.Boolean](JVMConfigurationKeys.INHERIT_MULTIFILE_PARTS, arguments.inheritMultifileParts)
    configuration.put[jl.Boolean](JVMConfigurationKeys.SKIP_RUNTIME_VERSION_CHECK, arguments.skipRuntimeVersionCheck)
    configuration.put[jl.Boolean](CLIConfigurationKeys.ALLOW_KOTLIN_PACKAGE, arguments.allowKotlinPackage)
    configuration.put[jl.Boolean](CLIConfigurationKeys.REPORT_PERF, arguments.reportPerf)
    configuration.put[jl.Boolean](JVMConfigurationKeys.USE_SINGLE_MODULE, arguments.singleModule)
    configuration.put[jl.Boolean](JVMConfigurationKeys.ADD_BUILT_INS_FROM_COMPILER_TO_DEPENDENCIES, arguments.addCompilerBuiltIns)
    configuration.put[jl.Boolean](JVMConfigurationKeys.CREATE_BUILT_INS_FROM_MODULE_DEPENDENCIES, arguments.loadBuiltInsFromDependencies)
    if (arguments.declarationsOutputPath != null)
      configuration.put[String](JVMConfigurationKeys.DECLARATIONS_JSON_PATH, arguments.declarationsOutputPath)
  }

  private def getClasspath(paths: KotlinPaths, arguments: K2JVMCompilerArguments): ju.List[File] = {
    val classpath = ListBuffer[File]()
    if (arguments.classpath != null) {
      classpath ++= arguments.classpath.split(File.pathSeparatorChar).map(new File(_))
    }
    if (!arguments.noStdlib) {
      classpath += paths.getRuntimePath
      classpath += paths.getScriptRuntimePath
    }
    // "-no-stdlib" implies "-no-reflect": otherwise we would be able to transitively read stdlib classes through kotlin-reflect,
    // which is likely not what user wants since s/he manually provided "-no-stdlib"
    if (!arguments.noReflect && !arguments.noStdlib)
      classpath += paths.getReflectPath
    classpath.asJava
  }

  private def createCoreEnvironment(rootDisposable: Disposable, configuration: CompilerConfiguration): KotlinCoreEnvironment = {
    KotlinCoreEnvironment.createForProduction(rootDisposable, configuration, EnvironmentConfigFiles.JVM_CONFIG_FILES)
  }

  private def setupJdkClasspathRoots(arguments: K2JVMCompilerArguments, configuration: CompilerConfiguration, messageCollector: MessageCollector): ExitCode = {
    try {
      if (!arguments.noJdk) {
        if (arguments.jdkHome != null) {
          messageCollector.report(CompilerMessageSeverity.LOGGING, s"Using JDK home directory ${arguments.jdkHome}", CompilerMessageLocation.NO_LOCATION)
          val classesRoots = PathUtil.getJdkClassesRoots(new File(arguments.jdkHome))
          if (classesRoots.isEmpty) {
            messageCollector.report(CompilerMessageSeverity.ERROR, s"No class roots are found in the JDK path: ${arguments.jdkHome}", CompilerMessageLocation.NO_LOCATION)
            return ExitCode.COMPILATION_ERROR
          }
          for (file <- classesRoots.asScala)
            configuration.add(JVMConfigurationKeys.CONTENT_ROOTS, new JvmClasspathRoot(file))
        } else for (file <- PathUtil.getJdkClassesRoots().asScala)
          configuration.add(JVMConfigurationKeys.CONTENT_ROOTS, new JvmClasspathRoot(file))
      } else if (arguments.jdkHome != null) {
        messageCollector.report(CompilerMessageSeverity.STRONG_WARNING, "The '-jdk-home' option is ignored because '-no-jdk' is specified", CompilerMessageLocation.NO_LOCATION)
      }
    } catch {
      case t: Throwable =>
        MessageCollectorUtil.reportException(messageCollector, t)
        ExitCode.INTERNAL_ERROR
    }
    ExitCode.OK
  }

}
