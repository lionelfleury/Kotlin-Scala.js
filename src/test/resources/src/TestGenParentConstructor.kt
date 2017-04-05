object Test {

    open class Up(val a: Int, val b : Int) {
        fun display() { println("$a $b") }
    }

    class Down(a: Int) : Up(a, 1) {

    }
    fun main() {
        val a = Down(5)
        a.display()
    }
}