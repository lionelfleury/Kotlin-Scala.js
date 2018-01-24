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
open class DummyClass

class MyClassWithExtension {
    fun get(i: Int): Double {
        return i.toDouble()
    }
}

fun MyClassWithExtension.dummy(a: Double) {
    println("this is another extension function")
}

fun MyClassWithExtension.dummy() {
    println("this is an extension function")
}

fun MyClassWithExtension.usingThis(): Double {
    var sum = 0.0
    for (i in 0..3) {
        sum += get(i)
    }

    return sum
}

fun String.myExtendedFun(): Int = 2

fun String.printWith(s: String) = println("$this $s")

object Test {

    fun String.count2(): Int {
        return length * 2
    }

    fun String.manyCount2(other: String, other1: String): Int {
        coucou()
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


        val ext = MyClassWithExtension()
        ext.dummy()
        ext.dummy(1.0)
        ext.usingThis()

        "Hello".printWith("World")
    }

    fun coucou() {

    }

}

object TestClassExtension {
    fun main(args: Array<String>) {
        Test.main()
    }
}