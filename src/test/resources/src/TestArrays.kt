object Test {
    fun main() {
        val a = arrayOf(1, 2, 3, 12.5)
        println(a.get(0))
        println(a.get(3))
        println(a.size)

        val c = a.iterator()

        while(c.hasNext()) println(c.next())

    }
}