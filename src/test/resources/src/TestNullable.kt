class DummyClass {
    var a1: Int = 0

    fun dummyFun() {
        println("Dummy function")
    }
    fun getSelf(): DummyClass {
        return this
    }

    fun getNullable(isNull: Boolean): DummyClass? {
        return if (isNull) null else this
    }
}

fun npeAssertWithCatch(a: Any?) {
    try {
        a!!
    } catch (e: NullPointerException) {
        println("Caught NPE")
    }
}

object TestNullable {
    fun main(args: Array<String>) {
        val e: Int? = if (true) 1 else null
        val f: DummyClass? = if (false) DummyClass() else null
        val g: Int? = null
        val h: Int? = if (false) null else 1

        println(e)
        println(f)
        println(g)
        println(h)

        val myDouble: Double? = 4.2
        val b: String? = "NonNull"
        val d: String? = null
        val myNull: DummyClass? = null
        val myNonNull: DummyClass = DummyClass()

        // Auto cast
        if (b != null) println(b.length)
        if (d != null) println(d.length)
        if (myDouble != null) myDouble.toInt() else null

        // Safe call operator
        val c: Int? = b?.length
        val nullVal = myNonNull.getNullable(true)?.a1
        val zeroVal = myNonNull.getNullable(false)?.a1

        println(nullVal)
        println(zeroVal)

        println(c)
        println(d?.length)

        println(myDouble?.toInt())

        // Elvis operator
        val l = b?.length ?: -1
        val q = d?.length ?: -1
        val r = myNull ?: DummyClass()
        val s = myNonNull.getNullable(true) ?: myNonNull.getSelf()

        r.dummyFun()
        s.dummyFun()

        println(l)
        println(q)
        println(b + d)

        // !! Operator
        npeAssertWithCatch(myNull)
        npeAssertWithCatch(d)
        npeAssertWithCatch(myNonNull)

        try {
            myNull!!.dummyFun()
        } catch (e: NullPointerException) {
            println("Null pointer caught successfully")
        }

        myNonNull!!.dummyFun()


    }
}