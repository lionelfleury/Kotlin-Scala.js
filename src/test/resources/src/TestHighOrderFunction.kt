object Test {

    fun toto(body: () -> Int ) : Int {
        return body() + 5
    }

    fun main() {
        println(toto {  -> 5 })
    }
}
