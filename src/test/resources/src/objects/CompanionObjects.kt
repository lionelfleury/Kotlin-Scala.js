class MyObjectClass(val inner: String) {
    var helloWorld = "Hello Nobody"
    init {
        doStuff()
    }

    companion object MyCompanion {
        val inner = "Hello from a companion object"
        // FIXME: 'val instance = MyCompanion' should be translated to 'val instance = this'
        val instance = MyObject.MyOtherObject
        fun getSome(a: Int): Int = a
    }

    object MyObject {
        val inner = "Hello from an object"

        object MyOtherObject {
            val inner = "Hello from an object within an object"
        }
    }

    fun new() = MyObjectClass("some new inner value")

    fun doStuff() {
        helloWorld = "Hello World with " + getSome(3)
    }

}

fun myfun() = MyObjectClass.MyObject