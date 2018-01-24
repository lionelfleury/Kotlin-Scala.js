fun IntArray.dummy(predicate: (kotlin.Int) -> kotlin.Boolean): IntArray {

    println(predicate(5))
    return intArrayOf(4, 5, 6, 7)
}

object MyHighOrderTest {

    fun addFive(body: () -> Double ) = body() + 5

    fun five() = 5.5

    fun double(x : Int) = x * 2

    fun isOdd(x: Int) = x % 2 != 0


    fun a(body: (Int, Int) -> Int) = body(1, 2) * body(3, 4)
    fun add(x: Int, y : Int) = x + y

    fun main() {
        // Test explicit invoke on function
        println((MyHighOrderTest::double).invoke(6))
        println((MyHighOrderTest::five).invoke())

        // Test invoke on function reference
        println(addFive(MyHighOrderTest::five))
        println(addFive {  -> 6.0 })

        val numbers = intArrayOf(1, 2, 3)
        val result = numbers.dummy(MyHighOrderTest::isOdd)
        println(result.size)

        // Many parameters
        println(a(MyHighOrderTest::add))
        println(a { x, y -> y - x })
    }
}

object TestHighOrderFunction {

    fun main(args: Array<String>) {
        MyHighOrderTest.main()
    }
}
