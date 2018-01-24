class MyClassA(val num: Int)

object TestBinaryOps {
    fun main(args: Array<String>) {
        val a = MyClassA(0)
        val b = MyClassA(0)
        val c = a
        val d = MyClassA(1)

        println(a === b)
        println(b === a)
        println(a == b)
        println(b == a)
        println(a === a)
        println(b === b)
        println(a === c)
        println(a !== b)
        println(a != d)

        println(1.0f)
        println(2 + 2)
        println(2 - 2)
        println(2 * 2)
        println(2 / 2)
        println(2 % 2)
        println(2 or 2)
        println(2 and 2)
        println(2 xor 2)
        println(2 shl 1)
        println(-10 shr 2)
        println(-10 ushr 2)

        println(true and true)
        println(true and false)

        println(true or false)
        println(true or true)

        println(2L + 2L)
        println(2L - 2L)
        println(2L * 2L)
        println(2L / 2L)
        println(2L % 2L)
        println(2L or 2L)
        println(2L and 2L)
        println(2L xor 2L)
        println(2L shl 1)
        println(-10L shr 2)
        println(-1000000L ushr 2)

        println(2.5 + 2.5)
        println(2.5 - 2.5)
        println(2.5 * 2.5)
        println(2.5 / 2.5)
        println(2.5 % 2.5)

        println(2.5f + 2.5f)
        println(2.5f - 2.5f)
        println(2.5f * 2.5f)
        println(2.5f / 2.5f)
        println(2.5f % 2.5f)

        println(2L + 2)
        println(100000L + 2147483647)
        println(2147483647L + 10000)


        val e = "First"
        val f = "Second"

        println(e + f)
    }
}