external class Math {
    companion object
}

fun main(args: Array<String>) {
    val value = 16.0
    val math : dynamic = Math
    println("The square root of $value is ${math.sqrt(value)}")
}

