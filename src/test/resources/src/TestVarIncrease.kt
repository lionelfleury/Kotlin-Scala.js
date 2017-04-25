object Test {
    var outter = 15

    fun main() {
        var sum = 0
        sum += 15
        Test.outter = sum + 15
        println(Test.outter)
    }
}