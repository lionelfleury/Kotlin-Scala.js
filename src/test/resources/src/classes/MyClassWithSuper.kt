open class MyParentWithSuper {
    open val a: Int = 0
    open var b: Int = 0
    open fun dummy(): Unit = println("dummy from parent")
}

open class MyOtherParentWithSuper {
    open fun f() = println("A")
}

open class MyClassWithSuper: MyParentWithSuper() {

    override val a: Int = 1
    override var b: Int = 0

    override fun dummy() {
        super.dummy()
        println("dummy from child")
    }

    fun printA() {
        println("var a from super = ${super.a}")
        println("var a from child = $a")
    }

    fun useSuperB() {
        super.b = 5
        println("var b from super = ${super.b}")
    }

    fun test() {
        dummy()
        printA()
        useSuperB()
    }
}

interface MyInterfaceWithSuper {
    fun f() = println("B")
}

class MyClassWithTwoParents : MyOtherParentWithSuper(), MyInterfaceWithSuper {
    override fun f() {
        super<MyOtherParentWithSuper>.f()
        super<MyInterfaceWithSuper>.f()
    }
}

interface MyOtherInterface {
    fun f() = println("MyOtherInterface")
}

object MyObjectWithSuper: MyInterfaceWithSuper, MyOtherInterface {
    override fun f() {
        println("MyObjectWithSuper.f")
        super<MyInterfaceWithSuper>.f()
        super<MyOtherInterface>.f()
    }
}