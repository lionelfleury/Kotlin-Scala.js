import java.io.File

import ch.epfl.k2sjsir.K2SJSIRCompiler


class BlackBoxAdvancedTests extends BlackBoxTest {

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

//  test("TestRanges.kt") {
//    val result = consoleToString {
//      val i = 0 to 10
//      println(i.head)
//      println(i.end)
//
//      Seq(0, 11, -12).foreach(x => println(i contains x))
//
//      println(true)
//      (0 to 10).foreach(println)
//    }
//    assertExecResult(result, "TestRanges.kt")
//  }

//  test("TestArrays.kt") {
//    val result = consoleToString {
//      val a = Seq(1, 2, 3, 12.5)
//      printlnJSFormat(a(0))
//      printlnJSFormat(a(3))
//      printlnJSFormat(a.size)
//      a.foreach(printlnJSFormat)
//
//      val b = Seq(12, 42)
//      b.foreach(println)
//    }
//    assertExecResult(result, "TestArrays.kt")
//  }


}
