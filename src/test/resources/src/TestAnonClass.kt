object Test {
    interface Top {
        fun display(): String
    }

    open class Bottom internal constructor() : Top {
        init {
            print("Hello ")
        }

        override fun display(): String = "World"
    }

    fun main() {
        val a = Bottom().display()
        println(a)
    }
}
fun main(args: Array<String>) {
    Test.main()
}
