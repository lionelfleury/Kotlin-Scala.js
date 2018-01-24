
fun hello() : Unit = println("Hello World")
val myVal = "A top-level value"
var myVar = "A top-level variable"

class TopLevelCaller {
    fun call() {
        println("Hey")
        hello()
        welcome("Toto")
    }
}

object TestTopLevelCalls {
    fun main(args: Array<String>) {
        val c = TopLevelCaller()
        c.call()

        hello()
        welcome("Kotlin")
        /*
    // FIXME: Cannot call the top-level properties yet
    println(myVal)
    println(myVar)
    myVar = "My top-level variable"
    println(myVar)
    */
    }
}