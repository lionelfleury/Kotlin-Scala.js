object Test {
    fun main() {
        val b : Any = 5
        println(b as Int + 10)

        val c : Number = 12.5
        println(c as Double)
    }
}