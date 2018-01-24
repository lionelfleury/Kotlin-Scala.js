import java.io.{ByteArrayOutputStream, File, PrintStream}

import ch.epfl.k2sjsir.K2SJSIRCompiler
import org.jetbrains.kotlin.cli.common.ExitCode
import org.scalajs.cli.Scalajsld
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, FunSuite}

import scala.sys.process._

trait BlackBoxTest extends FunSuite with BeforeAndAfter with BeforeAndAfterAll {

  protected val ROOT_SOURCE = "src/test/resources/src"
  protected val ROOT_LIB    = "src/test/resources/lib"
  protected val ROOT_OUT    = "src/test/resources/out"
  protected val ROOT_SRC_LIB= "src/test/resources/src/lib"
  protected val ROOT_LIB_OUT= "src/test/resources/kotlin-out"
  protected val SCALA_JS_VERSION = "1.0.0-M2"
  protected val SCALA_JS_JAR = "scalajs-library_2.12-"+ SCALA_JS_VERSION +".jar"

  protected val KOTLINJS_STD_LIB = ROOT_SOURCE + "/kotlin-stdlib"

  protected val KOTLIN_HOME = scala.util.Properties.envOrElse("KOTLIN_HOME", "/usr/share/kotlin" )

  private def cleanOutput(folder: File): Unit = {
    if (folder == null) {
      println("Folder is null")
      return
    }

    val files = folder.listFiles()

    if (files == null){
      println("No files to be deleted")
      return
    }

    val removedExtensions = Set(".sjsir", ".js", ".class")

    files.foreach { f =>
      if (f.isDirectory) {
        cleanOutput(f)
        f.delete()

      } else {
        val ext = f.getName.substring(f.getName.lastIndexOf("."))

        if (removedExtensions.contains(ext))
          f.delete()
      }
    }
  }

  before {
    cleanOutput(new File(ROOT_OUT))
  }

  protected def assertIrResult(expected: String, srcFile: String, mainClass: String = "Test$") = {
    new K2SJSIRCompiler()
      .exec(System.err, Array(s"$ROOT_SOURCE/$srcFile", "-Xallow-kotlin-package", "-d", ROOT_OUT, "-kotlin-home", KOTLIN_HOME):_*)
    val content = Scalajsp.run(Array(s"$ROOT_OUT/$mainClass.sjsir"))
    assert(expected == content)
  }

  protected def compileAndCheckIr(sources: Seq[String], mainClass: String = "", outFile: String = s"$ROOT_OUT/out.js", optimize: Boolean = true) = {
    val files = sources.map(s => s"$ROOT_SOURCE/$s")
    val options = Seq("-Xallow-kotlin-package", "-d", ROOT_OUT, "-kotlin-home", KOTLIN_HOME)

    val exitCode = new K2SJSIRCompiler()
      .exec(System.err,
        (files ++ options):_*)


    if (exitCode != ExitCode.OK)
      fail(s"Compilation process finished with status $exitCode")

    val mainMethod: List[String] = if (mainClass != "") List("-mm", mainClass) else Nil
    val linkerArgs: List[String] = List(
      "--stdlib", s"$ROOT_LIB/$SCALA_JS_JAR",
      ROOT_OUT, ROOT_LIB_OUT,
      "-o", s"$outFile",
      "-c",
      if (optimize) "-u" else "-n"
    )
    Scalajsld.main((linkerArgs ++ mainMethod).toArray)

    reportJsSize(new File(s"$outFile"))
  }

  protected def assertExecResult(expected: String, sources: Seq[String], outFile: String = "out.js", mainClass: String = "Test") = {
    val files = sources.map(s => s"$ROOT_SOURCE/$s")
    val options = Seq("-Xallow-kotlin-package", "-d", ROOT_OUT, "-kotlin-home", KOTLIN_HOME)

    val exitCode = new K2SJSIRCompiler()
      .exec(System.err,
        (files ++ options):_*)


    if (exitCode != ExitCode.OK)
      fail(s"Compilation process finished with status $exitCode")

    val linkerArgs = Array(
      ROOT_OUT, // Where to find the sjsir files
      "--stdlib", s"$ROOT_LIB/$SCALA_JS_JAR",
      ROOT_LIB_OUT,
      "-o", s"$ROOT_OUT/$outFile",
      "-c",
      "-mm", mainClass)

    Scalajsld.main(linkerArgs)

    val result = s"node $ROOT_OUT/$outFile".!!

    val expectedRes = expected.replaceAll("\\s+", "")
    val outputRes = result.replaceAll("\\s+", "")

    if(expectedRes != outputRes) {
      val sb = new StringBuffer()

      sb.append("\nOutput is different: (whitespace is always ignored)\n")
      sb.append("\nOutput\n" )
      sb.append("=========================\n")
      sb.append(result)
      sb.append("\nExpected output\n" )
      sb.append("=========================\n")
      sb.append(expected)

      fail(sb.toString)
    }
  }

  protected def printlnJSFormat(x: Any) = {
    x match {
      case d: Double => if(d%1 == 0) printf("%.0f\n", d) else println(d)
      case f: Float => if(f%1 == 0) printf("%.0f\n", f) else println(f)
      case _=> println(x)
    }
  }

  protected def consoleToString(thunk: => Unit) : String = {
    val baos = new ByteArrayOutputStream()
    val stream =  new PrintStream(baos)
    Console.withOut(stream)(thunk)
    baos.toString()
  }


  protected def reportJsSize(f: File): Unit = {
    println("Size of the file is " + f.length().toDouble / 1000.0 + " kB")
  }

}
