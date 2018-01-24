package benchmarks.sha512

import benchmarks.collections.LinkedList
import benchmarks.collections.sum

class MeanAndSEM(val mean: Double, val sem: Double) {
  override fun toString(): String {
    return mean.toString() + " us +- " + sem.toString() + " us"
  }
}

fun main(args: Array<String>) {
  BenchRunner.main(args)
}

object BenchRunner {
  fun main(args: Array<String>) {
    val result = report()
    println(result)
    /*
    val div = document.createElement("div")
    document.body!!.appendChild(div)
    val button = document.createElement("button") as HTMLButtonElement
    div.appendChild(button)
    val resultLabel = document.createElement("p")
    div.appendChild(resultLabel)

    button.appendChild(document.createTextNode("Run bench"))
    button.onclick = { _: Event ->
      resultLabel.clear()
      resultLabel.appendChild(document.createTextNode("Running ..."))

      val benchmarkResult = report()

      resultLabel.clear()
      resultLabel.appendChild(document.createTextNode(benchmarkResult))
    }
    document.body!!.appendChild(div)
    */
  }
  /** Run the benchmark the specified number of milliseconds and return
   *  the mean execution time and SEM in microseconds.
   */
  private fun runBenchmark(timeMinimum: Long, runsMinimum: Int): MeanAndSEM {
    var runs = 0
    var enoughTime: Boolean = false
    val stopTime = currentTimeMillis() + timeMinimum

    val samples = LinkedList<Double>()

    do {
      val startTime = currentTimeMillis()
      run()
      val endTime = currentTimeMillis()
      samples.add((endTime - startTime) * 1000.0)
      runs += 1
      enoughTime = endTime >= stopTime
    } while (!enoughTime || runs < runsMinimum)

    return meanAndSEM(samples)
  }

  private fun currentTimeMillis(): Double {
    return performance.now()
  }

  private fun meanAndSEM(samples: LinkedList<Double>): MeanAndSEM {
    val n = samples.size.toDouble()
    val mean = samples.sum() / n
    val sem = standardErrorOfTheMean(samples, mean)
    return MeanAndSEM(mean, sem)
  }

  private fun standardErrorOfTheMean(samples: LinkedList<Double>, mean: Double): Double {
    val n = samples.size.toDouble()
    var sumSqs = 0.0
    val iter = samples.iterator()
    while(iter.hasNext()) {
      val sample = iter.next().toDouble()
      sumSqs += (sample - mean) * (sample - mean)
    }
    return Math.sqrt(sumSqs / (n * (n - 1)))
  }

  private fun warmUp() {
    runBenchmark(100, 2)
  }
  private fun report(): String {
    warmUp()
    val measures = runBenchmark(2000, 5)
    return measures.toString()
  }

  private fun run() {
    if (!selfTest(true))
      throw Exception("Self test failed")
  }

  private fun aaas(): CharArray {
    val aaas = CharArray(1000)
    for (i in 0 until 1000)
      aaas[i] = 'a'
    return aaas
  }

  private fun asBytes(values: IntArray): ByteArray {
    val bytes = ByteArray(values.size)
    for (i in 0 until values.size)
      bytes[i] = values[i].toByte()

    return bytes
  }

  private fun getBytes(str: CharArray): ByteArray {
    val bytes = ByteArray(str.size)
    for (i in 0 until str.size) {
      val c = str[i]
      bytes[i] = c.toByte()
    }

    return bytes
  }
  /*
   * FIPS-180-2 test vectors
   */
  private val sha512TestBuf = arrayOf(
          getBytes(charArrayOf('a','b','c')),
          getBytes(charArrayOf('a','b','c','d','e','f','g','h','b','c','d','e','f','g','h','i','c','d','e','f','g','h','i','j','d','e','f','g','h','i','j','k','e','f','g','h','i','j','k','l','f','g','h','i','j','k','l','m','g','h','i','j','k','l','m','n','h','i','j','k','l','m','n','o','i','j','k','l','m','n','o','p','j','k','l','m','n','o','p','q','k','l','m','n','o','p','q','r','l','m','n','o','p','q','r','s','m','n','o','p','q','r','s','t','n','o','p','q','r','s','t','u')
          ),
          getBytes(aaas())
  )

  private val sha512TestSum = arrayOf(
          /*
           * SHA-384 test vectors
           */
          asBytes(intArrayOf(
                  0xCB, 0x00, 0x75, 0x3F, 0x45, 0xA3, 0x5E, 0x8B,
                  0xB5, 0xA0, 0x3D, 0x69, 0x9A, 0xC6, 0x50, 0x07,
                  0x27, 0x2C, 0x32, 0xAB, 0x0E, 0xDE, 0xD1, 0x63,
                  0x1A, 0x8B, 0x60, 0x5A, 0x43, 0xFF, 0x5B, 0xED,
                  0x80, 0x86, 0x07, 0x2B, 0xA1, 0xE7, 0xCC, 0x23,
                  0x58, 0xBA, 0xEC, 0xA1, 0x34, 0xC8, 0x25, 0xA7
          )),
          asBytes(intArrayOf(
                  0x09, 0x33, 0x0C, 0x33, 0xF7, 0x11, 0x47, 0xE8,
                  0x3D, 0x19, 0x2F, 0xC7, 0x82, 0xCD, 0x1B, 0x47,
                  0x53, 0x11, 0x1B, 0x17, 0x3B, 0x3B, 0x05, 0xD2,
                  0x2F, 0xA0, 0x80, 0x86, 0xE3, 0xB0, 0xF7, 0x12,
                  0xFC, 0xC7, 0xC7, 0x1A, 0x55, 0x7E, 0x2D, 0xB9,
                  0x66, 0xC3, 0xE9, 0xFA, 0x91, 0x74, 0x60, 0x39
          )),
          asBytes(intArrayOf(
                  0x9D, 0x0E, 0x18, 0x09, 0x71, 0x64, 0x74, 0xCB,
                  0x08, 0x6E, 0x83, 0x4E, 0x31, 0x0A, 0x4A, 0x1C,
                  0xED, 0x14, 0x9E, 0x9C, 0x00, 0xF2, 0x48, 0x52,
                  0x79, 0x72, 0xCE, 0xC5, 0x70, 0x4C, 0x2A, 0x5B,
                  0x07, 0xB8, 0xB3, 0xDC, 0x38, 0xEC, 0xC4, 0xEB,
                  0xAE, 0x97, 0xDD, 0xD8, 0x7F, 0x3D, 0x89, 0x85
          )),

          /*
           * SHA-512 test vectors
           */
          asBytes(intArrayOf(
                  0xDD, 0xAF, 0x35, 0xA1, 0x93, 0x61, 0x7A, 0xBA,
                  0xCC, 0x41, 0x73, 0x49, 0xAE, 0x20, 0x41, 0x31,
                  0x12, 0xE6, 0xFA, 0x4E, 0x89, 0xA9, 0x7E, 0xA2,
                  0x0A, 0x9E, 0xEE, 0xE6, 0x4B, 0x55, 0xD3, 0x9A,
                  0x21, 0x92, 0x99, 0x2A, 0x27, 0x4F, 0xC1, 0xA8,
                  0x36, 0xBA, 0x3C, 0x23, 0xA3, 0xFE, 0xEB, 0xBD,
                  0x45, 0x4D, 0x44, 0x23, 0x64, 0x3C, 0xE8, 0x0E,
                  0x2A, 0x9A, 0xC9, 0x4F, 0xA5, 0x4C, 0xA4, 0x9F
          )),
          asBytes(intArrayOf(
                  0x8E, 0x95, 0x9B, 0x75, 0xDA, 0xE3, 0x13, 0xDA,
                  0x8C, 0xF4, 0xF7, 0x28, 0x14, 0xFC, 0x14, 0x3F,
                  0x8F, 0x77, 0x79, 0xC6, 0xEB, 0x9F, 0x7F, 0xA1,
                  0x72, 0x99, 0xAE, 0xAD, 0xB6, 0x88, 0x90, 0x18,
                  0x50, 0x1D, 0x28, 0x9E, 0x49, 0x00, 0xF7, 0xE4,
                  0x33, 0x1B, 0x99, 0xDE, 0xC4, 0xB5, 0x43, 0x3A,
                  0xC7, 0xD3, 0x29, 0xEE, 0xB6, 0xDD, 0x26, 0x54,
                  0x5E, 0x96, 0xE5, 0x5B, 0x87, 0x4B, 0xE9, 0x09
          )),
          asBytes(intArrayOf(
                  0xE7, 0x18, 0x48, 0x3D, 0x0C, 0xE7, 0x69, 0x64,
                  0x4E, 0x2E, 0x42, 0xC7, 0xBC, 0x15, 0xB4, 0x63,
                  0x8E, 0x1F, 0x98, 0xB1, 0x3B, 0x20, 0x44, 0x28,
                  0x56, 0x32, 0xA8, 0x03, 0xAF, 0xA9, 0x73, 0xEB,
                  0xDE, 0x0F, 0xF2, 0x44, 0x87, 0x7E, 0xA6, 0x0A,
                  0x4C, 0xB0, 0x43, 0x2C, 0xE5, 0x77, 0xC3, 0x1B,
                  0xEB, 0x00, 0x9C, 0x5C, 0x2C, 0x49, 0xAA, 0x2E,
                  0x4E, 0xAD, 0xB2, 0x17, 0xAD, 0x8C, 0xC0, 0x9B
          ))
  )

  /*
   * Checkup routine
   */
  private fun selfTest(verbose: Boolean): Boolean {
    for (i in 0 until sha512TestSum.size) {
      val j = i % 3
      val is384 = i < 3

      if (verbose) {
        println("  SHA-" + (if (is384) 384 else 512) + " test " + (j + 1) + ": ")
      }

      val ctx = SHA512Context(is384)
      val buf = sha512TestBuf[j]

      if (j == 2) {
        for (x in 0 until 1000) {
          ctx.update(buf, buf.size)
        }
      } else {
        ctx.update(buf, buf.size)
      }

      val sha512sum = ByteArray(if(is384) 48 else 64)

      ctx.finish(sha512sum)


      if (!contentEquals(sha512sum, sha512TestSum[i])) {
        if (verbose)
          println("failed")
        return false
      }

      if (verbose)
        println("passed")
    }

    if (verbose)
      println("")

    return true
  }

  private fun contentEquals(a1: ByteArray, a2: ByteArray): Boolean {
    if (a1 === a2) return true
    if (a1.size != a2.size) return false

    for (i in 0 until a1.size) {
      if (a1[i] != a2[i]) {
        return false
      }
    }

    return true
  }
}

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
