package benchmarks

import benchmarks.collections.LinkedList
import benchmarks.collections.sum

/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Benchmarks        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2003-2016, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \                               **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

/** Simple benchmarking framework.
 *
 *  The `run` method has to be defined by the user, who will perform the
 *  timed operation there.
 *
 *  This will run the benchmark a minimum of 5 times and for at least 2
 *  seconds.
 */
abstract class Benchmark {

    open fun main(args: Array<String>): Unit {
        main()
    }

    fun main(): Unit {
        val status = report()
        println("$prefix: $status")
    }

    private val performanceTime: Double
        get() {
            return performance.now()
        }

    /** This method should be implemented by the concrete benchmark.
     *  It will be called by the benchmarking code for a number of times.
     *
     *  @see setUp
     *  @see tearDown
     */
    abstract fun run(): Unit

    /** Run the benchmark the specified number of milliseconds and return
     *  the mean execution time and SEM in microseconds.
     */
    fun runBenchmark(timeMinimum: Long, runsMinimum: Int): Pair {
        var runs = 0
        var enoughTime = false
        val stopTime = performanceTime + timeMinimum

        val samples = LinkedList<Double>()

        do {
            val startTime = performanceTime
            run()
            val endTime = performanceTime

            samples.add((endTime - startTime) * 1000.0)
            runs += 1
            enoughTime = endTime >= stopTime
        } while (!enoughTime || runs < runsMinimum)

        return meanAndSEM(samples)
    }

    private fun meanAndSEM(samples: LinkedList<Double>): Pair {
        val n = samples.size
        val mean = samples.sum() / n
        val sem = standardErrorOfTheMean(samples, mean)
        return Pair(mean, sem)
    }

    private fun standardErrorOfTheMean(samples: LinkedList<Double>, mean: Double): Double {
        val n = samples.size.toDouble()
        var sum = 0.0
        val iter = samples.iterator()
        while(iter.hasNext()) {
            val xi = iter.next().toDouble()
            sum += Math.pow(xi - mean, 2.0)
        }

        return Math.sqrt(sum / (n * (n - 1)))
    }

    /** Prepare any data needed by the benchmark, but whose execution time
     *  should not be measured. This method is run before each call to the
     *  benchmark payload, 'run'.
     */
    fun setUp(): Unit {}

    /** Perform cleanup operations after each 'run'. For micro benchmarks,
     *  think about using the result of 'run' in a way that prevents the JVM
     *  to dead-code eliminate the whole 'run' method. For instance, print or
     *  write the results to a file. The execution time of this method is not
     *  measured.
     */
    fun tearDown(): Unit { }

    /** A string that is written at the beginning of the output line
     *  that contains the timings. By default, this is the class name.
     */
    open val prefix: String = "Benchmark"

    fun warmUp(): Unit {
        runBenchmark(1000, 10)
    }

    open fun report(): String {
        setUp()
        warmUp()
        val pair = runBenchmark(3000, 20)
        val mean = pair.left
        val sem = pair.right
        tearDown()

        return "$mean us +- $sem us"
    }
}

class Pair(val left: Double, val right: Double)

external class Math {
    companion object {
        fun sqrt(n: Double): Double
        fun pow(a: Double, b: Double): Double
    }
}

external class performance {
    companion object {
        fun now(): Double
    }
}