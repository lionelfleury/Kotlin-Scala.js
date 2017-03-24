import java.io.{ByteArrayOutputStream, File, PrintStream, StringWriter}

import ch.epfl.k2sjsir.K2SJSIRCompiler
import org.jetbrains.kotlin.cli.common.CLICompiler
import org.scalatest.{BeforeAndAfter, FunSuite}

import scala.sys.process._

class BlackBoxTest extends FunSuite with BeforeAndAfter {

  private val ROOT_SOURCE = "src/test/resources/src"
  private val ROOT_LIB    = "src/test/resources/lib"
  private val ROOT_OUT    = "src/test/resources/out"

  private val KOTLIN_HOME = scala.util.Properties.envOrElse("KOTLIN_HOME", "/usr/share/kotlin" )

  after {
    cleanOutput()
  }

  private def cleanOutput() = {
    val folder = new File(ROOT_OUT)
    val files = folder.listFiles()
    files.foreach(f => if (f.getName.endsWith(".sjsir") || f.getName.endsWith(".js")) f.delete())
  }

  private def assertIrResult(expected: String, srcFile: String, mainClass: String = "Test$") = {
    CLICompiler.doMainNoExit(new K2SJSIRCompiler(), Array(s"$ROOT_SOURCE/$srcFile", "-d", ROOT_OUT, "-kotlin-home", KOTLIN_HOME))
    val content = Scalajsp.run(Array(s"$ROOT_OUT/$mainClass.sjsir"))
    assert(expected == content)
  }

  private def assertExecResult(expected: String, srcFile: String, outFile: String = "out.js", mainClass: String = "Test") = {
    CLICompiler.doMainNoExit(new K2SJSIRCompiler(), Array(s"$ROOT_SOURCE/$srcFile", "-d", ROOT_OUT, "-kotlin-home", KOTLIN_HOME))
    Scalajsld.run(Array("--stdlib", s"$ROOT_LIB/scalajs-library_2.12-0.6.15.jar", ROOT_OUT, "-o", s"$ROOT_OUT/$outFile"))
    val success = (s"echo $mainClass().main()" #>> new File(s"$ROOT_OUT/$outFile")).!
    if(success == 0) {
      val result = s"node $ROOT_OUT/$outFile".!!
      if(expected.replaceAll("\\s+", "") != result.replaceAll("\\s+", "") ) {
        val sb = new StringBuffer()
        sb.append("\nOutput is different: (whitespace is always ignored)")
        sb.append("\noutput: \n")
        sb.append(result)
        sb.append("\nexpected output: \n")
        sb.append(expected)
        assert(false, sb.toString)
      }
    } else {
      fail("Unable to append line to file")
    }
  }

  private def printlnJSFormat(x: Any) = {
    x match {
      case d: Double => if(d%1 == 0) printf("%.0f\n", d) else println(d)
      case f: Float => if(f%1 == 0) printf("%.0f\n", f) else println(f)
      case _=> println(x)
    }
  }

  test("TestSimplePrint.kt should output 1") {
    assertExecResult("1", "TestSimplePrint.kt")
  }

  test("TestBinaryOps.kt should compute the correct result") {
    val scalaResult = {
      val baos = new ByteArrayOutputStream()
      val stream =  new PrintStream(baos)
      Console.withOut(stream) {

        printlnJSFormat(2 + 2)
        printlnJSFormat(2 - 2)
        printlnJSFormat(2 * 2)
        printlnJSFormat(2 / 2)
        printlnJSFormat(2 % 2)
        printlnJSFormat(2 | 2)
        printlnJSFormat(2 & 2)
        printlnJSFormat(2 ^ 2)
        printlnJSFormat(2 << 1)
        printlnJSFormat(-10 >> 2)
        printlnJSFormat(-10 >>> 2)

        printlnJSFormat(true & true)
        printlnJSFormat(true & false)

        printlnJSFormat(true | false)
        printlnJSFormat(true | true)

        printlnJSFormat(2L + 2L)
        printlnJSFormat(2L - 2L)
        printlnJSFormat(2L * 2L)
        printlnJSFormat(2L / 2L)
        printlnJSFormat(2L % 2L)
        printlnJSFormat(2L | 2L)
        printlnJSFormat(2L & 2L)
        printlnJSFormat(2L ^ 2L)
        printlnJSFormat(2L << 1)
        printlnJSFormat(-10L >> 2)
        printlnJSFormat(-1000000L >>> 2)

        printlnJSFormat(2.5 + 2.5)
        printlnJSFormat(2.5 - 2.5)
        printlnJSFormat(2.5 * 2.5)
        printlnJSFormat(2.5 / 2.5)
        printlnJSFormat(2.5 % 2.5)

        printlnJSFormat(2.5f + 2.5f)
        printlnJSFormat(2.5f - 2.5f)
        printlnJSFormat(2.5f * 2.5f)
        printlnJSFormat(2.5f / 2.5f)
        printlnJSFormat(2.5f % 2.5f)

        printlnJSFormat(2L + 2)
        printlnJSFormat(100000L + 2147483647)
        printlnJSFormat(2147483647L + 10000)
      }
      baos.toString()
    }
    assertExecResult(scalaResult, "TestBinaryOps.kt")

  }
}
