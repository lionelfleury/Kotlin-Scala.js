import java.io.{ByteArrayOutputStream, File, PrintStream}

import ch.epfl.k2sjsir.K2SJSIRCompiler
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, FunSuite}

import scala.sys.process._

class BlackBoxTest extends FunSuite with BeforeAndAfter with BeforeAndAfterAll {

  private val ROOT_SOURCE = "src/test/resources/src"
  private val ROOT_LIB    = "src/test/resources/lib"
  private val ROOT_OUT    = "src/test/resources/out"
  private val ROOT_SRC_LIB= "src/test/resources/src/lib"
  private val ROOT_LIB_OUT= "src/test/resources/kotlin-out"

  private val KOTLIN_HOME = scala.util.Properties.envOrElse("KOTLIN_HOME", "/usr/share/kotlin" )

  override def beforeAll = {
    new K2SJSIRCompiler()
      .exec(System.err, Array(ROOT_SRC_LIB, "-d", ROOT_LIB_OUT, "-kotlin-home", KOTLIN_HOME, "-Xallow-kotlin-package"):_*)
  }

  private def deleteRecursively(file: File): Unit = {
    if (file.isDirectory)
      file.listFiles.foreach(deleteRecursively)
    file.delete()
  }

  override def afterAll = {
    deleteRecursively(new File(ROOT_LIB_OUT))
  }

  after {
    cleanOutput(new File(ROOT_OUT))
  }

  private def cleanOutput(folder: File) = {
    val files = folder.listFiles()
    files.foreach(f =>
      if (f.getName.endsWith(".sjsir") || f.getName.endsWith(".js") || f.getName.endsWith(".class")) f.delete())
  }

  private def assertIrResult(expected: String, srcFile: String, mainClass: String = "Test$") = {
    new K2SJSIRCompiler()
      .exec(System.err, Array(s"$ROOT_SOURCE/$srcFile", "-d", ROOT_OUT, "-kotlin-home", KOTLIN_HOME):_*)
    val content = Scalajsp.run(Array(s"$ROOT_OUT/$mainClass.sjsir"))
    assert(expected == content)
  }

  private def assertExecResult(expected: String, srcFile: String, outFile: String = "out.js", mainClass: String = "Test") = {
    new K2SJSIRCompiler()
      .exec(System.err, Array(s"$ROOT_SOURCE/$srcFile", "-d", ROOT_OUT, "-kotlin-home", KOTLIN_HOME):_*)
    Scalajsld.run(Array("--stdlib", s"$ROOT_LIB/scalajs-library_2.12-0.6.15.jar", ROOT_OUT, ROOT_LIB_OUT, "-o", s"$ROOT_OUT/$outFile"))
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

  private def consoleToString(thunk: => Unit) : String = {
    val baos = new ByteArrayOutputStream()
    val stream =  new PrintStream(baos)
    Console.withOut(stream)(thunk)
    baos.toString()
  }

  test("TestBinaryOps.kt should compute the correct result") {
    val scalaResult = consoleToString {
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

      val e = "First"
      val f = "Second"

      println(e + f)
    }
    assertExecResult(scalaResult, "TestBinaryOps.kt")

  }

  test("TestEqualities.kt should be accurate") {
    val result = consoleToString {
      val a = 1
      println(a == 1)
      println(1 == 2)
      println(2.0 == 3.0)
      println(true == false)
      println(false == false)

      println(a != 1)
      println(1 != 2)
      println(2.0 != 3.0)
      println(true != false)
      println(false != false)

      println(!true)
      println(!false)

      println("a" == "b")
      println("a" == "a")
    }
    assertExecResult(result, "TestEqualities.kt")
  }

  test("TestNullable.kt") {
    assertExecResult(
      """7
        |7
        |null
        |7
        |-1
        |NonNullnull""".stripMargin, "TestNullable.kt")
  }


  test("TestTryCatch.kt") {
    assertExecResult(
      """Exception caught
        |Reached finally""".stripMargin, "TestTryCatch.kt")
  }

  test("TestClassExtension.kt") {
    assertExecResult(
      """
        |4
        |8
        |8
        |16
      """.stripMargin, "TestClassExtension.kt")
  }

  test("TestStringConcat.kt") {
    assertExecResult("5 Hello World", "TestStringConcat.kt")
  }

  test("TestAnonClass.kt") {
    assertExecResult("Hello World", "TestAnonClass.kt")
  }

  test("TestRanges.kt") {
    val result = consoleToString {
      val i = 0 to 10
      println(i.head)
      println(i.end)

      Seq(0, 11, -12).foreach(x => println(i contains x))

      println(true)
      (0 to 10).foreach(println)
    }
    assertExecResult(result, "TestRanges.kt")
  }

  test("TestGenParentConstructor.kt") {
    assertExecResult("5 1", "TestGenParentConstructor.kt")
  }

  test("TestAssignationOrder.kt") {
    assertExecResult(
      """
        |true
        |6
        |12
        |false
        |5
        |11
      """.stripMargin, "TestAssignationOrder.kt")
  }

  test("TestTypeCast.kt") {
    assertExecResult(
      """
        |15
        |12.5
      """.stripMargin, "TestTypeCast.kt")
  }

  test("TestArrays.kt") {
    val result = consoleToString {
      val a = Seq(1, 2, 3, 12.5)
      printlnJSFormat(a(0))
      printlnJSFormat(a(3))
      printlnJSFormat(a.size)
      a.foreach(printlnJSFormat)

      val b = Seq(12, 42)
      b.foreach(println)
    }
    assertExecResult(result, "TestArrays.kt")
  }
}
