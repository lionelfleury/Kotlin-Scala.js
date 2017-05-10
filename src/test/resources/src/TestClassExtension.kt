/*fun String.count(): Int {
    return length
}

fun String.manyCount(other: String): Int {
    return count() + other.count()
}*/


class D {
    fun bar() { println("D.bar") }
}

class C {
    fun baz() { println("C.baz") }

    fun D.foo() {
        bar()   // calls D.bar
        baz()   // calls C.baz
    }

    fun caller(d: D) {
        d.foo()   // call the extension function
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
        val a : String = "Toto"
        val other: String = "Titito"

        /*val c = a.count2()
        println(c)*/
        /*val d = a.manyCount(other)*/
        //val e = a.manyCount2(other, other)

        //println(b)
        //println(c)
        /*println(d)*/
        //println(e)

        val c = C()
        val d = D()

        c.caller(d)

    }



}