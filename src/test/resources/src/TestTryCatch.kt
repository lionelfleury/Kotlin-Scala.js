fun main(args: Array<String>) {
    try {
        throw Exception()
    } catch(e: Exception) {
        println("Exception caught")
    } finally {
        println("Reached finally")
    }
}