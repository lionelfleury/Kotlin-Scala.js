object Test {

    open class Up(val a: Int, val b : Int) {
        fun display() { println("$a $b") }
    }

    class Down(c: Int) : Up(c, 1) {
    }

    fun main() {
        val a = Down(5)
        a.display()
    }
}