open class Base (val p: Int) {

    override fun toString(): String {
        return "Base with p = $p"
    }
}

class Derived(p: Int) : Base(p) {

    fun doSomething(): Unit = println("Doing something !")

    override fun toString(): String {
        return "Derived with p = $p"
    }
}

object TestIsOperator {
    fun main(args: Array<String>) {
        val myDouble = 1.7
        val myInt = 4
        val myLong = 23432542300000
        val myDerived = Derived(myInt)
        val myBase = Base(myInt)

        check(myDouble)
        check(myInt)
        check(myLong)
        check(myDerived)
        check(myBase)

        useWithoutSmartCast(myDerived)
        useWithoutSmartCast("hello world")
    }
}

fun check(a: Any) {

    if (a is Int) println("$a is an Int")
    if (a is Double) println("$a is a Double")
    if (a is Long) println("$a is a Long")


    if (a !is Int) println("$a is not an Int")
    if (a !is Double) println("$a is not a Double")
    if (a !is Long) println("$a is not a Long")

    if (a is Base) println("$a is Base class")
    if (a is Derived) println("$a is Derived class")
    if (a !is Derived) println("$a is not Derived class")
}

fun useWithoutSmartCast(a: Any) {
    if (a is Derived) {
        val a2 = a as Derived
        a2.doSomething()
    } else if (a is String) {
        val a3 = a as String
        println(a3 + " is a String")
    }
}