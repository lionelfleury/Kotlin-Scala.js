class KotlinStdLibTest extends BlackBoxTest {

  val builtins = Seq(
    "kotlin-stdlib/core/builtins/native/kotlin/Comparable.kt",
    "kotlin-stdlib/js/js.libraries/src/builtins"
  )

  val main = Seq(
    "kotlin-stdlib/libraries/stdlib/js/build/builtin-sources",
    "kotlin-stdlib/js/js.libraries/src/browser",
    "kotlin-stdlib/js/js.libraries/src/core",
    "kotlin-stdlib/js/js.libraries/src/deprecated",
    "kotlin-stdlib/js/js.libraries/src/dom",
    "kotlin-stdlib/js/js.libraries/src/generated",
    "kotlin-stdlib/js/js.libraries/src/js",
    "kotlin-stdlib/js/js.libraries/src/reflect",
    "kotlin-stdlib/libraries/stdlib/js/build/common-sources"
  )
  /*
  test("Compile Kotlin JS Standard Library v1.1.61") {
    compileStdLib
  }

  test("Compile stdlib and run some code") {
    compileStdLib
    assertExecResult("Hello World", Seq("kotlin-stdlib/HelloWorld.kt"), mainClass = "HelloWorldKt")
  }
  */

  private def compileStdLib = {

    compileAndCheckIr(builtins)
    compileAndCheckIr(main)
  }
}
