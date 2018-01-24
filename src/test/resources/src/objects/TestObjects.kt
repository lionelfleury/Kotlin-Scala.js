
object SomeObject {
    val inner = "someobject inner value"

    val some = inner
}


object TestObjects {
    fun main(args: Array<String>) {
        // Object
        val obj = SomeObject
        println(obj.inner)

        // Anonymous object
        val a = AnonymousObjects()
        a.bar()

        val tmp = MyObjectClass("My inner value")
        println(tmp.helloWorld)

        // Companion objects
        println(myfun().inner)
        println(MyObjectClass.inner)
        println(MyObjectClass("some initial value").new().new().inner)
        println(MyObjectClass.instance.inner)
        println(MyObjectClass.MyObject.inner)
        println(MyObjectClass.MyObject.MyOtherObject.inner)

    }
}