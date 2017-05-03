import java.io.{ByteArrayOutputStream, File, PrintStream}

import ch.epfl.k2sjsir.K2SJSIRCompiler
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, FunSuite}

import scala.sys.process._

trait BlackBoxTest extends FunSuite with BeforeAndAfter with BeforeAndAfterAll {

  protected val ROOT_SOURCE = "src/test/resources/src"
  protected val ROOT_LIB    = "src/test/resources/lib"
  protected val ROOT_OUT    = "src/test/resources/out"
  protected val ROOT_SRC_LIB= "src/test/resources/src/lib"
  protected val ROOT_LIB_OUT= "src/test/resources/kotlin-out"

  protected val KOTLIN_HOME = scala.util.Properties.envOrElse("KOTLIN_HOME", "/usr/share/kotlin" )

  private def cleanOutput(folder: File) = {
    val files = folder.listFiles()
    files.foreach(f =>
      if (f.getName.endsWith(".sjsir") || f.getName.endsWith(".js") || f.getName.endsWith(".class")) f.delete())
  }

  after {
    cleanOutput(new File(ROOT_OUT))
  }

  protected def assertIrResult(expected: String, srcFile: String, mainClass: String = "Test$") = {
    new K2SJSIRCompiler()
      .exec(System.err, Array(s"$ROOT_SOURCE/$srcFile", "-d", ROOT_OUT, "-kotlin-home", KOTLIN_HOME):_*)
    val content = Scalajsp.run(Array(s"$ROOT_OUT/$mainClass.sjsir"))
    assert(expected == content)
  }

  protected def assertExecResult(expected: String, srcFile: String, outFile: String = "out.js", mainClass: String = "Test") = {
    new K2SJSIRCompiler()
      .exec(System.err, Array(s"$ROOT_SOURCE/$srcFile", "-d", ROOT_OUT, "-kotlin-home", KOTLIN_HOME):_*)
    Scalajsld.run(Array("--stdlib", s"$ROOT_LIB/scalajs-library_2.12-0.6.15.jar", ROOT_OUT, ROOT_LIB_OUT, "-o", s"$ROOT_OUT/$outFile", "-c"))
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

}
