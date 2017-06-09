open class Toto(b: Int) {

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

object Test : Toto(5, 7.0) {

    var b = 6

    fun main() {
        val a = 5
        println("b $b")
        println(a)
    }

}

fun main(args: Array<String>) {
    Test.main()
}