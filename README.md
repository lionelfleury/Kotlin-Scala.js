# Kotlin-Scala.js 

[![Build Status](https://travis-ci.org/flonso/Kotlin-Scala.js.svg?branch=master)](https://travis-ci.org/flonso/Kotlin-Scala.js)

Compile Kotlin code to JavaScript through the Scala.js compiler

## Documentation
A full technical report is available under the [docs/report](https://github.com/flonso/Kotlin-Scala.js/tree/master/docs/report) directory.
Useful resources include the [Kotlin Documentation](https://kotlinlang.org/docs/reference/) and the [Scala.js Website](https://www.scala-js.org/).

## Running the compiler
```bash
sbt "run {list_of_source_files} -d {output_sjsir_dir} -kotlin-home {kotlin_home_directory}"
```
To compile the Kotlin Standard Library use the `-Xallow-kotlin-package` option to ignore the `package kotlin` reservation.

- A gradle plugin is also available with an example project [here](https://github.com/flonso/kotlin-scalajs-gradle-plugin)
- A basic maven plugin is available [here](https://github.com/ex0ns/kotlin-scalajs-maven-plugin)

 
## Testing

All tests are stored under `src/test` and can be run with `sbt test`.
Before running them, create the folders `src/test/resources/out` and `src/test/resources/benchmark_out`.

You will need to install the same version of Kotlin which is used by SBT.

To do so, either install it from here https://kotlinlang.org/docs/tutorials/command-line.html and/or change the path
to the directory by changing the KOTLIN_HOME value in BlackBoxTest.scala

The tests require the presence of the Scala.js library jar file inside `src/test/resources/lib`. It should be included in this repository.
If you need to change the Scala.js version in use for the tests, be sure to change the referenced `.jar` inside the `BlackBoxTest.scala` file.

### Kotlin Standard Library
A test has been setup to check that the IR generated for the files constituting the Kotlin Standard Library is valid.
Because the standard library is not compilable yet, the test has been disabled by default. It can be enabled in file `src/test/scala/KotlinStdLibTest`.

### Benchmarking
Benchmarks are available in `src/test/scala/BenchmarkTest`. Outputs are written to the `benchmark_out` directory mentioned above.