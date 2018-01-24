
class BlackBoxSimpleTests extends BlackBoxTest {

  test("TestDom.kt") {
    compileAndCheckIr(Seq("dom"), "TestDom.main")
  }

  test("TestLoops.kt") {
    val result =
      """
        |55
        |30
        |45
        |20
        |55
        |4
      """.stripMargin

    assertExecResult(result, Seq("loops"), mainClass = "loops.TestLoops.main")
  }

  test("TestObjects.kt") {
    val result =
      """
        |someobject inner value
        |nothing to be done, inside anonymous object
        |printing something
        |publicFoo.AnonymousObjects$NoNameProvided@1 foo.x publicFooWithClass.x publicFooWithInterface.x  (42, Hello World)
        |Hello World with 3
        |Hello from an object
        |Hello from a companion object
        |some new inner value
        |Hello from an object within an object
        |Hello from an object
        |Hello from an object within an object
      """.stripMargin
    assertExecResult(result, Seq("objects"), mainClass = "TestObjects.main")
  }

  test("TestClasses.kt") {
    val result =
      """
        |ZERO
        |ONE
        |ZERO
        |Exception correctly raised
        |ONE
        |Zero is Zero !
        |42 32 42
        |Hello World
        |dummy from parent
        |dummy from child
        |var a from super = 0
        |var a from child = 1
        |var b from super = 5
        |A
        |B
        |MyObjectWithSuper.f
        |B
        |MyOtherInterface
        |First initializer block that prints Toto
        |Second initializer block that prints 4
        |First property: Toto
        |Second property: 4
      """.stripMargin

    assertExecResult(result, Seq("classes/"), mainClass = "TestClasses.main")
  }

  test("TestInterface.kt") {
    val result =
      """(a = 0 b = Hello World c = Hello World dog = Dog(Rex))
        |(a = 0 b = Hello World c = Some value dog = Dog(Rex))
        |I bought a dog ! His name is Rex.
        |Barking !
        |You pet your dog !
        |Barking !
        |Your dog is moving his tail in happiness
        |And some more prints
        |(a = 0 b = Hello World c = Some value dog = Dog(Lassie))
        |(a = 0 b = Hello World c = Hey there. dog = Dog(Lassie))
        |10 is a great number
        |It's something !
        |I'm a var but I cannot be set without an override !
        |This value overrides the one from the interface
      """.stripMargin
    assertExecResult(result, Seq("interfaces/"), mainClass = "TestInterface.main")
  }

  test("TestJsFunc.kt") {
    val res = consoleToString {
      println(1)
      println(3)
      println(6)

      def _asJsNotation(d: Double): String = {
        return d.toString.replace("E", "e+").replace("+-", "-")
      }

      println(_asJsNotation(Double.MaxValue) + " "
        + "5e-324" + " "
        + Double.PositiveInfinity + " "
        + Double.NegativeInfinity + " "
        + Double.NaN
      )

      println(_asJsNotation(Float.MaxValue) + " "
        + _asJsNotation(Float.MinPositiveValue) + " "
        + Float.PositiveInfinity + " "
        + Float.NegativeInfinity + " "
        + Float.NaN + " "
      )

      println("9223372036854775807" + " "
        + "-9223372036854775808"
      )
    }

    assertExecResult(res, Seq("TestJsFunc.kt"), mainClass = "TestJsFunc.main")
  }

  test("TestAccessorsGen.kt") {
    val res = consoleToString {

      class MyClassWithAccessors {
        private var _myVar: Int = 0
        def myVar: Int = _myVar
        def myVar_=(value: Int) = {
          if (value >= 2)
            _myVar = value - 1
          else
            _myVar = -1
        }

        val _name: String = "name"
        def name: String = _name

        val id: Int = 1

        private var _tmp: String = "tmp"

        def tmp = _tmp
        def tmp_= (value: String): Unit = {
          _tmp = value + "_set"
        }
      }


      val a = new MyClassWithAccessors

      printlnJSFormat(a.id)
      printlnJSFormat(a.name)
      printlnJSFormat(a.myVar)
      a.myVar = 4
      printlnJSFormat(a.myVar)
      a.myVar = 0
      printlnJSFormat(a.myVar)
      printlnJSFormat(a.tmp)
      a.tmp = "tmp2"
      printlnJSFormat(a.tmp)
      printlnJSFormat("customSet\nnot_using_default_parameter_name_set")
    }

    assertExecResult(res, Seq("TestAccessorsGen.kt"), mainClass = "TestAccessorsGen.main")
  }

  test("TestThisKeyword.kt") {
    val res =
      """
        |myDef
        |true
        |myDef
        |false
        |Calling this.myDef() from extension function : myDef
        |Calling myDef() (no this) from extension function : myDef
        |Using this in an extension : this.number = 3
        |I was called using this from an interface default implementation
        |I was called using this from an interface default implementation
        |I accessed this.b !
      """.stripMargin
    assertExecResult(res, Seq("TestThisKeyword.kt"), mainClass = "TestThisKeyword.main")
  }

  test("TestIsOperator.kt") {
    val expectedResult =
      """
        |1.7 is a Double
        |1.7 is not an Int
        |1.7 is not a Long
        |1.7 is not Derived class
        |4 is an Int
        |4 is a Double
        |4 is not a Long
        |4 is not Derived class
        |23432542300000 is a Long
        |23432542300000 is not an Int
        |23432542300000 is not a Double
        |23432542300000 is not Derived class
        |Derived with p = 4 is not an Int
        |Derived with p = 4 is not a Double
        |Derived with p = 4 is not a Long
        |Derived with p = 4 is Base class
        |Derived with p = 4 is Derived class
        |Base with p = 4 is not an Int
        |Base with p = 4 is not a Double
        |Base with p = 4 is not a Long
        |Base with p = 4 is Base class
        |Base with p = 4 is not Derived class
        |Doing something !
        |hello world is a String
      """.stripMargin

    assertExecResult(expectedResult, Seq("TestIsOperator.kt"), mainClass = "TestIsOperator.main")
  }

  // FIXME: println Float and Double with decimal points
  test("TestMethodsBaseTypes.kt") {
    val expectedResult = {
    """
      |1000000
      |1234567890123456
      |999999999
      |4293713502
      |3530134674
      |=== Type cast : from Int to X ===
      |Int 65
      |Byte 65
      |Short 65
      |Long 65
      |Float 65
      |Double 65
      |Char A
      |=== Type cast : from Byte to X ===
      |Byte 65
      |Int 65
      |Short 65
      |Long 65
      |Float 65
      |Double 65
      |Char A
      |=== Type cast : from Short to X ===
      |Short 65
      |Int 65
      |Byte 65
      |Long 65
      |Float 65
      |Double 65
      |Char A
      |=== Type cast : from Long to X ===
      |Long 65
      |Int 65
      |Byte 65
      |Short 65
      |Float 65
      |Double 65
      |Char A
      |=== Type cast : from Float to X ===
      |Float 65.4000015258789
      |Int 65
      |Byte 65
      |Short 65
      |Long 65
      |Double 65.4000015258789
      |Char A
      |=== Type cast : from Double to X ===
      |Double 65.7
      |Int 65
      |Byte 65
      |Short 65
      |Float 65.69999694824219
      |Long 65
      |Char A
      |=== Type cast : from Char to X ===
      |Char A
      |Int 65
      |Byte 65
      |Short 65
      |Float 65
      |Long 65
      |Double 65
    """.stripMargin
    }

    assertExecResult(expectedResult, Seq("TestMethodsBaseTypes.kt"), mainClass = "TestMethodsBaseTypes.main")
  }

  test("TestSimplePrint.kt") {
    assertExecResult("1", Seq("TestSimplePrint.kt"), mainClass = "TestSimplePrint.main")
  }

  test("TestBinaryOps.kt") {
    val scalaResult = consoleToString {
      class MyClassA(val num: Int)

      val a = new MyClassA(0)
      val b = new MyClassA(0)
      val c = a
      val d = new MyClassA(1)

      printlnJSFormat(a === b)
      printlnJSFormat(b === a)
      printlnJSFormat(a == b)
      printlnJSFormat(b == a)
      printlnJSFormat(a === a)
      printlnJSFormat(b === b)
      printlnJSFormat(a === c)
      printlnJSFormat(a !== b)
      printlnJSFormat(a != d)

      printlnJSFormat(1.0f)
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
    assertExecResult(scalaResult, Seq("TestBinaryOps.kt"), mainClass = "TestBinaryOps.main")
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
      """.stripMargin, Seq("TestMultipleConstructors.kt"), mainClass = "TestMultipleConstructors.main")
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
    assertExecResult(result, Seq("TestEqualities.kt"), mainClass = "TestEqualities.main")
  }

  test("TestIf.kt") {
    assertExecResult(
      """
        |1
        |1
        |1
        |1
        |1
      """.stripMargin, Seq("TestIf.kt"), mainClass = "TestIf.main")
  }

  test("TestNullable.kt") {
    assertExecResult(
      """
        |1
        |null
        |null
        |1
        |7
        |null
        |0
        |7
        |null
        |4
        |Dummy function
        |Dummy function
        |7
        |-1
        |NonNullnull
        |Caught NPE
        |Caught NPE
        |Null pointer caught successfully
        |Dummy function
      """.stripMargin, Seq("TestNullable.kt"), mainClass = "TestNullable.main")
  }


  test("TestTryCatch.kt") {
    assertExecResult(
      """Exception caught
        |Reached finally""".stripMargin, Seq("TestTryCatch.kt"), mainClass = "TestTryCatch.main")
  }

  test("TestClassExtension.kt") {
    assertExecResult(
      """
        |8
        |32
        |D.bar
        |C.baz
        |this is an extension function
        |this is another extension function
        |Hello World
      """.stripMargin, Seq("TestClassExtension.kt"), mainClass = "TestClassExtension.main")
  }

  test("TestTopClassExtension.kt") {
    assertExecResult(
      """
        |10
      """.stripMargin, Seq("TestTopClassExtension.kt"), mainClass = "TestTopClassExtension.main")
  }

  test("TestStringConcat.kt") {
    assertExecResult("5 Hello World", Seq("TestStringConcat.kt"), mainClass = "TestStringConcat.main")
  }

  test("TestAnonClass.kt") {
      assertExecResult("Hello World", Seq("TestAnonClass.kt"), mainClass = "TestAnonClass.main")
  }


  test("TestGenParentConstructor.kt") {
    assertExecResult("5 1", Seq("TestGenParentConstructor.kt"), mainClass = "TestGenParentConstructor.main")
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
      """.stripMargin, Seq("TestAssignationOrder.kt"), mainClass = "TestAssignationOrder.main")
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
      """.stripMargin, Seq("TestHighOrderFunction.kt"), mainClass = "TestHighOrderFunction.main")
  }

    test("TestLambdaTopLevel.kt") {
      assertExecResult(
        """
          |11
        """.stripMargin, Seq("TestLambdaTopLevel.kt"), mainClass = "TestLambdaTopLevel.main")
  }

  test("TestLambda.kt") {
    assertExecResult(
      """
        |I'm in the body of a local lambda expression
        |Nothing, I'm a function
        |Hello World
        |2
        |I'm a lambda defined with fun keyword
        |I'm the body of a lambda with no param and no ret type
        |I'm a lambda with no return type and string param
        |I'm the body of a lambda with no param and ret type string
        |Lambda with params and ret string : 1 plus 0.42 is 1.42
        |Class property called with implicit receiver
        |Class property called with explicit this
        |Called with implicit receiver
        |Called with explicit this
      """.stripMargin, Seq("TestLambda.kt"), mainClass = "TestLambda.main")
  }

  test("TestTypeCast.kt") {
    assertExecResult(
      """
        |15
        |MyCastClass(0)
      """.stripMargin, Seq("TestTypeCast.kt"), mainClass = "TestTypeCast.main")
  }

  test("TestArraysBase.kt") {
    val result = consoleToString {
      val a = Seq(1, 2, 3, 12.5)
      printlnJSFormat(a(0))
      printlnJSFormat(a(3))
      printlnJSFormat(a.size)

      println(3)
    }
    assertExecResult(result, Seq("TestArraysBase.kt"), mainClass = "TestArraysBase.main")
  }

  /*
  test("TestArrayIterator.kt") {
    val result = consoleToString {
      val a = Seq(3, 2, 3, 12.5)
      a.foreach(printlnJSFormat)

      val b = Seq(12, 42)
      b.foreach(println)
    }
    assertExecResult(result, "TestArrayIterator.kt", mainClass = "TestArrayIteratorKt")
  }*/

  test("TestVarIncrease.kt") {
    assertExecResult(
      """
        |30
        |16
        |60
        |60
        |6
      """.stripMargin, Seq("TestVarIncrease.kt"), mainClass = "TestVarIncrease.main")
  }



  test("TestDynamic.kt") {
    assertExecResult("2", Seq("TestDynamic.kt"), mainClass = "TestDynamic.main")
  }

  test("TestTopLevelCalls.kt") {
    assertExecResult(
      """
        |Hey
        |Hello World
        |Hello Toto
        |Hello World
        |Hello Kotlin
      """.stripMargin, Seq("TestTopLevelCalls.kt", "TopLevelFunctions.kt"), mainClass = "TestTopLevelCalls.main")
  }

}

