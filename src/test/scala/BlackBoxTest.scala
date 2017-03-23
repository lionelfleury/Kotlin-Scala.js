import java.io.File

import ch.epfl.k2sjsir.K2SJSIRCompiler
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
    K2SJSIRCompiler.main(Array(s"$ROOT_SOURCE/$srcFile", "-d", ROOT_OUT, "-kotlin-home", KOTLIN_HOME))
    val content = Scalajsp.run(Array(s"$ROOT_OUT/$mainClass.sjsir"))
    assert(expected == content)
  }

  private def assertExecResult(expected: String, srcFile: String, outFile: String = "out.js", mainClass: String = "Test") = {
    K2SJSIRCompiler.main(Array(s"$ROOT_SOURCE/$srcFile", "-d", ROOT_OUT, "-kotlin-home", KOTLIN_HOME))
    Scalajsld.run(Array("--stdlib", s"$ROOT_LIB/scalajs-library_2.12-0.6.15.jar", ROOT_OUT, "-o", s"$ROOT_OUT/$outFile"))
    val success = (s"echo $mainClass().main()" #>> new File(s"$ROOT_OUT/$outFile")).!
    if(success == 0) {
      val result = s"node $ROOT_OUT/$outFile".!!
      assert(expected.replaceAll("\\s+", "") == result.replaceAll("\\s+", "") )
    } else {
      fail("Unable to append line to file")
    }
  }

  test("A simple print should display 1") {
    assertExecResult("1", "TestSimplePrint.kt")
  }

}
