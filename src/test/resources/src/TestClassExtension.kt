fun String.count(): Int {
    return length
}

fun String.manyCount(other: String): Int {
    return count() + other.count()
}

object Test {

    fun String.count2(): Int {
        return length * 2
    }

    fun String.manyCount2(other: String): Int {
        return count2() + other.count2()
    }

    fun main() {
        val a : String = "Toto"
        val other: String = "Titi"

        val b = a.count()
        val c = a.count2()
        val d = a.manyCount(other)
        val e = a.manyCount2(other)

        println(b)
        println(c)
        println(d)
        println(e)
    }
}