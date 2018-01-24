
class MyInterfacedClass(override var c: String) : MyInterface  {
    override var dog: Dog = Dog("Rex")
    override val a: Int = 0
    override val b: String = "Hello World"
    override var withOverride: String = super.withOverride

    fun tmp(d: Dog) {
        d.bark()
    }

    override fun petYourDog() {
        println("You pet your dog !")
        dog.bark()
        dog.beHappy()
    }

    override fun abstractMethod(arg1: Int, arg2: String): String {
        val ret = (arg1 * 5).toString() + " " + arg2

        return ret
    }

    override fun changeCToSomeValue() {
        c = "Some value"
    }

    override fun toString(): String {
        return "(a = " + a + " b = " + b + " c = " + c + " dog = " + dog + ")"
    }

    override fun assignSomeValues() {
        c = "Hey there."
    }
}