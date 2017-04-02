object Test {
    fun main() {
        try {
            throw Exception()
        } catch(e: Exception) {
            println("Exception caught")
        } finally {
            println("Reached finally")
        }
    }
}