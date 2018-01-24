//FIXME: Interface declaration signature is wrong
interface I1 {
    fun a()
}

class C1 : I1 {
    override fun a() {
        println("Hello")
    }
}


object TestInterface {
    fun main(args: Array<String>) {
        /*
        val c1 = C1()
        c1.a()
        val i: I1 = C1()
        i.a()
        */

        val myClass = MyInterfacedClass("Hello World")
        println(myClass)

        myClass.changeCToSomeValue()
        println(myClass)

        myClass.buyADog()

        myClass.dog.bark()
        myClass.petYourDog()
        myClass.dummyFun()

        myClass.dog = Dog("Lassie")
        println(myClass)

        myClass.assignSomeValues()
        println(myClass)
        println(myClass.abstractMethod(2, "is a great number"))
        myClass.doSomething()

        myClass.noOverride = "This value will be ignored"
        println(myClass.noOverride)

        myClass.withOverride = "This value overrides the one from the interface"
        println(myClass.withOverride)

    }
}