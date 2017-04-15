fun IntArray.dummy(predicate: (kotlin.Int) -> kotlin.Boolean): IntArray {
    //predicate(5)
    return intArrayOf(4,5,6,7)
}

object Test {

    /*fun toto(body: () -> Int ) : Int {
        return body() + 5
    }*/

    //fun cst() = 5
    //fun double(i: Int) = 2*i

    fun isOdd(x: Int) = x % 2 != 0

    fun main() {
        /*println(toto(Test::cst))
        println(toto {  -> 5 })*/

        val numbers = intArrayOf(1, 2, 3)
        val result = numbers.dummy(Test::isOdd)
        println(result.size)
    }
}
