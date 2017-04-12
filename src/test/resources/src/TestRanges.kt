object Test {
    fun main() {
        val a = IntRange(0, 10)
        println(a.start)
        println(a.endInclusive)
        println(a.contains(0))
        println(a.contains(11))
        println(a.contains(-12))

        val b = a.iterator()
        println(b.hasNext())

        for(i in a)  {
            println(i)
        }
    }
}