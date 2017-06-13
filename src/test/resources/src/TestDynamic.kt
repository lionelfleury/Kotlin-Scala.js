external class Math {
    companion object {
        fun sqrt(i: Double): Double
    }
}

fun main(args: Array<String>) {
    val math : dynamic = Math
    println(math.sqrt(4.0))
    println(Math.sqrt(16.0))
}