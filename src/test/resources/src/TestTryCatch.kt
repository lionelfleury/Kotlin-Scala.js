object TestTryCatch {
    fun main(args: Array<String>) {
        try {
            // FIXME: Check without string param
            throw Exception("FIXME")
        } catch (e: Exception) {
            println("Exception caught")
        } finally {
            println("Reached finally")
        }
    }
}