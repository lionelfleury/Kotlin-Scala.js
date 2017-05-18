package ch.epfl.k2sjsir

import com.sampullara.cli.Argument
import org.jetbrains.kotlin.cli.common.arguments.K2JsArgumentConstants.{CALL, NO_CALL}
import org.jetbrains.kotlin.cli.common.arguments.{CommonCompilerArguments, DefaultValues, GradleOption, ValueDescription}


@SerialVersionUID(0L)
class K2SJSIRCompilerArguments extends CommonCompilerArguments {

  @Argument(value = "d", description = "Destination for generated class files")
  @ValueDescription("<directory|jar>")
  var destination: String = _

  @Argument(value = "classpath", alias = "cp", description = "Paths where to find user class files")
  @ValueDescription("<path>")
  var classpath: String = _

//  @GradleOption(classOf[DefaultValues.BooleanFalseDefault])
//  @Argument(value = "include-runtime", description = "Include Kotlin runtime in to resulting .jar")
//  var includeRuntime = false

//  @GradleOption(classOf[DefaultValues.StringNullDefault])
//  @Argument(value = "jdk-home", description = "Path to JDK home directory to include into classpath, if differs from default JAVA_HOME")
//  @ValueDescription("<path>")
//  var jdkHome = null

//  @GradleOption(classOf[DefaultValues.BooleanFalseDefault])
//  @Argument(value = "no-jdk", description = "Don't include Java runtime into classpath")
//  var noJdk = false

  @GradleOption(classOf[DefaultValues.BooleanTrueDefault])
  @Argument(value = "no-stdlib", description = "Don't include Kotlin runtime into classpath")
  var noStdlib: Boolean = false

//  @GradleOption(classOf[DefaultValues.BooleanTrueDefault])
//  @Argument(value = "no-reflect", description = "Don't include Kotlin reflection implementation into classpath")
//  var noReflect = false

//  @Argument(value = "module", description = "Path to the module file to compile")
//  @ValueDescription("<path>")
//  var module = null

//  @Argument(value = "script", description = "Evaluate the script file")
//  var script = false

//  @Argument(value = "script-templates", description = "Script definition template classes")
//  @ValueDescription("<fully qualified class name[,]>")
//  var scriptTemplates = null

  @Argument(value = "kotlin-home", description = "Path to Kotlin compiler home directory, used for runtime libraries discovery")
  @ValueDescription("<path>")
  var kotlinHome: String = _

//  @Argument(value = "module-name", description = "Module name")
//  var moduleName = null

//  @GradleOption(classOf[DefaultValues.JvmTargetVersions])
//  @Argument(value = "jvm-target", description = "Target version of the generated JVM bytecode (1.6 or 1.8), default is 1.6")
//  @ValueDescription("<version>")
//  var jvmTarget = null

//  @GradleOption(classOf[DefaultValues.BooleanFalseDefault])
//  @Argument(value = "java-parameters", description = "Generate metadata for Java 1.8 reflection on method parameters")
//  var javaParameters = false

  // Advanced options
//  @Argument(value = "Xno-call-assertions", description = "Don't generate not-null assertion after each invocation of method returning not-null")
//  var noCallAssertions = false
//
//  @Argument(value = "Xno-param-assertions", description = "Don't generate not-null assertions on parameters of methods accessible from Java")
//  var noParamAssertions = false
//
//  @Argument(value = "Xno-optimize", description = "Disable optimizations")
//  var noOptimize = false
//
//  @Argument(value = "Xreport-perf", description = "Report detailed performance statistics")
//  var reportPerf = false

//  @Argument(value = "Xmultifile-parts-inherit", description = "Compile multifile classes as a hierarchy of parts and facade")
//  var inheritMultifileParts = false
//
//  @Argument(value = "Xskip-metadata-version-check", description = "Load classes with bad metadata version anyway (incl. pre-release classes)")
//  var skipMetadataVersionCheck = false
//
//  @Argument(value = "Xskip-runtime-version-check", description = "Allow Kotlin runtime libraries of incompatible versions in the classpath")
//  var skipRuntimeVersionCheck = false
//
//  @Argument(value = "Xdump-declarations-to", description = "Path to JSON file to dump Java to Kotlin declaration mappings")
//  @ValueDescription("<path>")
//  var declarationsOutputPath = null
//  @Argument(value = "Xsingle-module", description = "Combine modules for source files and binary dependencies into a single module")
//  var singleModule = false
//
//  @Argument(value = "Xadd-compiler-builtins", description = "Add definitions of built-in declarations to the compilation classpath (useful with -no-stdlib)")
//  var addCompilerBuiltIns = false
//
//  @Argument(value = "Xload-builtins-from-dependencies", description = "Load definitions of built-in declarations from module dependencies, instead of from the compiler")
//  var loadBuiltInsFromDependencies = false

  /* Added from K2JSCompilerArguments */
  @Argument(value = "libraries", description = "Paths to Kotlin libraries with .meta.js and .kjsm files, separated by system file separator")
  @ValueDescription("<path>")
  var libraries: String = _

  @GradleOption(classOf[DefaultValues.JsMain])
  @Argument(value = "main", description = "Whether a main function should be called")
  @ValueDescription("{" + CALL + "," + NO_CALL + "}")
  var main: String = _

  // Paths to output directories for friend modules.
  var friendPaths: Array[String] = _

  override def executableScriptFileName = ""

}
