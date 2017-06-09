object Up {
    var other = 15
}

class Down(var b : Int) {
}

object Test {
    var outter = 15

    fun main() {
        val a = Down(5)
        var sum = 15
        sum += 15
        println(sum)

        Test.outter = sum + 15
        Test.outter += 15
        Up.other = 16

        a.b = 6

        println(Up.other)
        println(Test.outter)
        println(outter)
        println(a.b)
    }
}

fun main(args: Array<String>) {
    Test.main()
}