/**
 * Example taken from kotlin website (https://kotlinlang.org/docs/reference/extensions.html)
 */
class D {
    fun bar() { println("D.bar") }
}

class C {
    fun baz() { println("C.baz") }

    fun D.foo() {
        bar()
        baz()
    }

    fun caller(d: D) {
        d.foo()
    }
}

object Test {

    fun String.count2(): Int {
        return length * 2
    }

    fun String.manyCount2(other: String, other1: String): Int {
        return count2() + other.count2() + other1.count2()
    }

    fun main() {
        val content = "Toto"
        val otherContent = "Titito"

        val doubleLength = content.count2()
        println(doubleLength)
        val mixLengths = content.manyCount2(otherContent, otherContent)

        println(mixLengths)

        val c = C()
        val d = D()

        c.caller(d)

    }

}