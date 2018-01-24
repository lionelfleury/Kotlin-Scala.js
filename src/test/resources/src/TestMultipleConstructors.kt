open class Toto(b: Int) {

    //FIXME: Omitting explicit call to this() (for class with no explicit primary constructor) leads to infinite loop
    constructor(b: Int, c: Int) :  this(b) {
        println(b)
        println(c)
    }

    constructor(b: Int, c: Double) :  this(b, 9) {
        println(b)
        println(c)
    }

    init {
        println("Init $b")
    }
}

class EmptyClass(val a: Int)

object Test : Toto(5, 7.0) {

    var b = 6

    fun main() {
        val a = 5
        println("b $b")
        println(a)
    }

}

object TestMultipleConstructors {
    fun main(args: Array<String>) {
        Test.main()
    }
}