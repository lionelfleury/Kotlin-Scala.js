object TestJsFunc {
    fun main(args: Array<String>) {
        val c = MyClassForJsFunc()

        c.dummy()
    }
}

class MyClassForJsFunc {
    fun dummy() {
        /*
        // This won't work, since we use a matching on the string itself :
        val str = "Kotlin.identityHashCode"
        js(str)(this)
        */
        val res1 = js("Kotlin.identityHashCode")(this)
        val res2 = js("Kotlin.identityHashCode")(3)
        val res3 = js("Kotlin.identityHashCode")(1 + 2 + 3)

        val d1: Double = js("Number.MAX_VALUE")
        val d2: Double = js("Number.MIN_VALUE")
        val d3: Double = js("Number.POSITIVE_INFINITY")
        val d4: Double = js("Number.NEGATIVE_INFINITY")
        val d5: Double = js("Number.NaN")


        val f1: Float = js("Number.MAX_VALUE")
        val f2: Float = js("Number.MIN_VALUE")
        val f3: Float = js("Number.POSITIVE_INFINITY")
        val f4: Float = js("Number.NEGATIVE_INFINITY")
        val f5: Float = js("Number.NaN")


        val l1: Long = js("Kotlin.Long.MAX_VALUE")
        val l2: Long = js("Kotlin.Long.MIN_VALUE")

        println(res1)
        println(res2)
        println(res3)

        println("" + d1 + " " + d2 + " " + d3+ " " + d4+ " " + d5)
        println("" + f1+ " " + f2+ " " + f3+ " " + f4+ " " + f5)
        println("" + l1+ " " + l2)
    }
}