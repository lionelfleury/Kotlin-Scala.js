class Other(first: Int, last: Int, val step: Int) {
    val next : Boolean = step > 0
    val low = if(next) step + first else step + last
    val up = low + last
}

object Test {
    fun main() {
        val a = Other(5, 6, 1)
        println(a.next)
        println(a.low)
        println(a.up)

        val b = Other(5, 6, -1)
        println(b.next)
        println(b.low)
        println(b.up)
    }
}