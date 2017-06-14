

class BlackBoxSimpleTests extends BlackBoxTest {

  test("TestSimplePrint.kt") {
    assertExecResult("1", "TestSimplePrint.kt", mainClass = "TestSimplePrintKt")
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
    assertExecResult(scalaResult, "TestBinaryOps.kt", mainClass = "TestBinaryOpsKt")
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
      """.stripMargin, "TestMultipleConstructors.kt", mainClass = "TestMultipleConstructorsKt")
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
    assertExecResult(result, "TestEqualities.kt", mainClass = "TestEqualitiesKt")
  }

  test("TestIf.kt") {
    assertExecResult(
      """
        |1
        |1
        |1
      """.stripMargin, "TestIf.kt", mainClass = "TestIfKt")
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
        |Reached finally""".stripMargin, "TestTryCatch.kt", mainClass = "TestTryCatchKt")
  }

  test("TestClassExtension.kt") {
    assertExecResult(
      """
        |8
        |32
        |D.bar
        |C.baz
      """.stripMargin, "TestClassExtension.kt", mainClass = "TestClassExtensionKt")
  }

  test("TestTopClassExtension.kt") {
    assertExecResult(
      """
        |10
      """.stripMargin, "TestTopClassExtension.kt", mainClass = "TestTopClassExtensionKt")
  }

  test("TestStringConcat.kt") {
    assertExecResult("5 Hello World", "TestStringConcat.kt", mainClass = "TestStringConcatKt")
  }

  test("TestAnonClass.kt") {
      assertExecResult("Hello World", "TestAnonClass.kt", mainClass = "TestAnonClassKt")
  }


  test("TestGenParentConstructor.kt") {
    assertExecResult("5 1", "TestGenParentConstructor.kt", mainClass = "TestGenParentConstructorKt")
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
      """.stripMargin, "TestAssignationOrder.kt", mainClass = "TestAssignationOrderKt")
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
      """.stripMargin, "TestHighOrderFunction.kt", mainClass = "TestHighOrderFunctionKt")
  }

    test("TestLambdaTopLevel.kt") {
      assertExecResult(
        """
          |11
        """.stripMargin, "TestLambdaTopLevel.kt", mainClass = "TestLambdaTopLevelKt")
  }

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
    assertExecResult(result, "TestArraysBase.kt", mainClass = "TestArraysBaseKt")
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
      """.stripMargin, "TestVarIncrease.kt", mainClass = "TestVarIncreaseKt")
  }



  test("TestDynamic.kt") {
    assertExecResult(
      """
        |2
        |4
        |3.141592653589793
        |3.141592653589793
      """.stripMargin, "TestDynamic.kt", mainClass = "TestDynamicKt")
  }

  test("TestTopLevelCalls.kt") {
    assertExecResult(
      """
        |Hello World
        |Hello Kotlin
      """.stripMargin, "TestTopLevelCalls.kt", mainClass = "TestTopLevelCallsKt")
  }

  test("TestAdvancedDynamic.kt") {
    assertExecResult(
      """
        |The square root of 16 is 4
      """.stripMargin, "TestAdvancedDynamic.kt", mainClass = "TestAdvancedDynamicKt"
    )
  }

}

