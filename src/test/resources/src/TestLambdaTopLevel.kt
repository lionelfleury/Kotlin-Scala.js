object Test {
    fun addFive(body: () -> Int ) = body() + 5

    fun main() {
        topLevel()
    }
}

fun topLevel() = println(Test.addFive {  -> 6 })


fun main(args: Array<String>) {
    Test.main()
}