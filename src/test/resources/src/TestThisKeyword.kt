interface MyThisInterface {
    var b: String

    val defaultAccessors: String
        get() {
            println(this.b)
            return "I accessed this.b !"
        }

    fun myThisInterfaceFun() {
        this.b = "I was called using this from an interface default implementation"
        println(this.b)
    }
}

class MyThisClass(val num: Int): MyThisInterface {
    override var b: String = ""

    var number: Int = 0

    fun myDef(): Unit {
        println("myDef")
    }

    fun equals(b: MyThisClass): Boolean {
        this.myDef()
        return this.num == b.num
    }
}

fun MyThisClass.myExtensionWithThis(a: Int) {
    print("Calling this.myDef() from extension function : ")
    this.myDef()
    print("Calling myDef() (no this) from extension function : ")
    myDef()
    this.number = a
    println("Using this in an extension : this.number = ${this.number}")
}

object TestThisKeyword {
    fun main(args: Array<String>) {
        val a = MyThisClass(1)
        val b = MyThisClass(1)
        val c = MyThisClass(2)

        println(a.equals(b))
        println(a.equals(c))

        a.myExtensionWithThis(3)
        a.myThisInterfaceFun()
        println(a.defaultAccessors)
    }
}