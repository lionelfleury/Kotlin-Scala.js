class MyCastClass {
    val a = 0

    override fun toString(): String {
        return "MyCastClass($a)"
    }
}

object TestTypeCast {
    fun main(args: Array<String>) {
        val b: Any = 5
        println(b as Int + 10)

        val c: Any = MyCastClass()
        println(c as MyCastClass)
    }
}