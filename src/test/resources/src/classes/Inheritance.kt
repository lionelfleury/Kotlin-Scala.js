open class MyAClass(val a: Int, val b: String) {

    class A1: MyAClass(0, "coucou"){}
    class A2: MyAClass(1, "hey") {}
    class A3: MyAClass(2, "hello") {}
}

class MyBClass(val c: Int) : MyAClass(c,"32") {

}

open class MyEmptyClass {
    fun dummy() = println("Hello World")
}

class B2 : MyEmptyClass()