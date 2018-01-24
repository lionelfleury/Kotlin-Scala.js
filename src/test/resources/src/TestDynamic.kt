external class Math {
    companion object {
        fun sqrt(i: Double): Double
    }
}

object TestDynamic {

    fun main(args: Array<String>) {
        println(Math.sqrt(4.0))
    }
}