fun myEnumTests() {
    // Test values()
    val values = MyEnumClass.values()
    for (v in values) {
        println(v)
    }

    // Test valueOf
    val valueOfZero = MyEnumClass.valueOf("ZERO")
    println(valueOfZero)

    try {
        val valueOfUnknow = MyEnumClass.valueOf("Unknown")
        println(valueOfUnknow)
    } catch (e: IllegalStateException) {
        println("Exception correctly raised")
    }

    // Accessing members
    val one = MyEnumClass.ONE
    println(one)

    if (valueOfZero == MyEnumClass.ZERO)
        println("Zero is Zero !")
    else
        println("Zero is not Zero !?")

    // Test enum with constructor
    //val tmp = Fruits.BANANA
    //println(tmp.name)
}

fun myInheritanceTest() {
    val a = MyAClass(0, "hey")
    val b = MyBClass(42)
    println("${b.a} ${b.b} ${b.c}")

    val empty = B2()
    empty.dummy()
}

fun myClassWithSuperTest() {
    val c = MyClassWithSuper()
    c.test()
    val c2 = MyClassWithTwoParents()
    c2.f()

    MyObjectWithSuper.f()
}

fun myDataClassTest() {
    //val r = Result(0, 1.0, "mystring")
    //r.test()
}

fun myMultitpleInit() {
    val i = InitOrderDemo("Toto")
    println(i.firstProperty)
    println(i.secondProperty)
}

object TestClasses {
    fun main(args: Array<String>) {
        myEnumTests()
        myInheritanceTest()
        myClassWithSuperTest()
        //myDataClassTest()
        myMultitpleInit()
    }
}