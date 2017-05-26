object Test {
    fun main() {
        val a = arrayOf(1, 2, 3, 12.5)
        a[0] = 3
        val c = a.iterator()

        while(c.hasNext()) println(c.next())

        val b = longArrayOf(12, 42)
        val d = b.iterator()
        while(d.hasNext()) println(d.next())
    }
}