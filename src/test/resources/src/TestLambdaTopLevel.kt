object LambdaTopLevel {
    fun addFive(body: () -> Int ) = body() + 5

    fun main() {
        topLevel()
    }
}

fun topLevel() = println(LambdaTopLevel.addFive {  -> 6 })


object TestLambdaTopLevel {
    fun main(args: Array<String>) {
        LambdaTopLevel.main()
    }
}