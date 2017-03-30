object Test {
    fun main() {
        val a : Int? = 1
        val b : String? = "NonNull"
        val c : String? = "Twice"
        val d : String? = null

        println(a)
        if(b != null) println(b.length)
        /*if(c != null) println(c.length)*/

        println(b + c + d)
    }
}
