

class BlackBoxSimpleTests extends BlackBoxTest {

  test("TestSimplePrint.kt") {
    assertExecResult("1", "TestSimplePrint.kt")
  }

  test("TestBinaryOps.kt") {
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

  test("TestMultipleConstructors.kt") {
    assertExecResult(
      """
        |Init 5
        |5
        |9
        |5
        |7
        |b 6
        |5
      """.stripMargin, "TestMultipleConstructors.kt")
  }

  test("TestEqualities.kt") {
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

  test("TestIf.kt") {
    assertExecResult(
      """
        |1
        |1
        |1
      """.stripMargin, "TestIf.kt")
  }

//  test("TestNullable.kt") {
//    assertExecResult(
//      """7
//        |7
//        |null
//        |7
//        |-1
//        |NonNullnull""".stripMargin, "TestNullable.kt")
//  }


  test("TestTryCatch.kt") {
    assertExecResult(
      """Exception caught
        |Reached finally""".stripMargin, "TestTryCatch.kt")
  }

  test("TestClassExtension.kt") {
    assertExecResult(
      """
        |8
        |32
        |D.bar
        |C.baz
      """.stripMargin, "TestClassExtension.kt")
  }

  test("TestTopClassExtension.kt") {
    assertExecResult(
      """
        |10
      """.stripMargin, "TestTopClassExtension.kt")
  }

  test("TestStringConcat.kt") {
    assertExecResult("5 Hello World", "TestStringConcat.kt")
  }

//  test("TestAnonClass.kt") {
//    assertExecResult("Hello World", "TestAnonClass.kt")
//  }


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

  test("TestHighFunction.kt") {
    assertExecResult(
      """
        |12
        |5.5
        |10.5
        |11
        |true
        |4
        |21
        |1
      """.stripMargin, "TestHighOrderFunction.kt")
  }

//  test("TestLambdaTopLevel.kt") {
//    assertExecResult(
//      """
//        |11
//      """.stripMargin, "TestLambdaTopLevel.kt")
//  }

//  test("TestTypeCast.kt") {
//    assertExecResult(
//      """
//        |15
//        |12.5
//      """.stripMargin, "TestTypeCast.kt")
//  }

  test("TestArraysBase.kt") {
    val result = consoleToString {
      val a = Seq(1, 2, 3, 12.5)
      printlnJSFormat(a(0))
      printlnJSFormat(a(3))
      printlnJSFormat(a.size)

      println(3)
    }
    assertExecResult(result, "TestArraysBase.kt")
  }

  /*test("TestArrayIterator.kt") {
    val result = consoleToString {
      val a = Seq(3, 2, 3, 12.5)
      a.foreach(printlnJSFormat)

      val b = Seq(12, 42)
      b.foreach(println)
    }
    assertExecResult(result, "TestArrayIterator.kt")
  }*/

  test("TestVarIncrease.kt") {
    assertExecResult(
      """
        |30
        |16
        |60
        |60
        |6
      """.stripMargin, "TestVarIncrease.kt")
  }

  test("TestDynamic.kt") {
    assertExecResult("2", "TestDynamic.kt")
  }


}
