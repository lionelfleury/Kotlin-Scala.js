fun IntArray.dummy(predicate: (kotlin.Int) -> kotlin.Boolean): IntArray {
    println(predicate(5))
    return intArrayOf(4, 5, 6, 7)
}

object Test {

    fun addFive(body: () -> Double ) = body() + 5

    fun five() = 5.5
    fun double(x : Int) = x * 2

    fun isOdd(x: Int) = x % 2 != 0


    fun a(body: (Int, Int) -> Int) = body(1, 2) * body(3, 4)
    fun add(x: Int, y : Int) = x + y

    fun main() {
        // Test explicit invoke on function
        println((Test::double).invoke(6))
        println((Test::five).invoke())

        // Test invoke on function reference
        println(addFive(Test::five))
        println(addFive {  -> 6.0 })

        val numbers = intArrayOf(1, 2, 3)
        val result = numbers.dummy(Test::isOdd)
        println(result.size)

        // Many parameters
        println(a(Test::add))
        println(a { x, y -> y - x })
    }
}
