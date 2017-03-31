object Test {
    fun main() {
        val b : String? = "NonNull"
        val d : String? = null

        if(b != null) println(b.length)
        if(d != null) println(d.length)

        val c = b?.length
        println(c)
        println(d?.length)

        val l = b?.length ?: -1
        val q = d?.length ?: -1

        println(l)
        println(q)

        println(b + d)
    }
}
