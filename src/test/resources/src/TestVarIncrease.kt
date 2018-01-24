object Up {
    var other = 15
}

class Down(var b : Int) {
}

object MyTestObj {
    var outter = 15

    fun main() {
        val a = Down(5)
        var sum = 15
        sum += 15
        println(sum)

        MyTestObj.outter = sum + 15

        MyTestObj.outter += 15
        Up.other = 16

        a.b = 6

        println(Up.other)
        println(MyTestObj.outter)
        println(outter)
        println(a.b)

    }
}

object TestVarIncrease {
    fun main(args: Array<String>) {
        MyTestObj.main()
    }
}