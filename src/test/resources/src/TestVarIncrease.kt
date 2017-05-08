object Toto {
    var other = 15
}

class Down(val b : Int) {

}
object Test {
    var outter = 15

    fun main() {
        val a = Down(5)
        var sum = 0
        sum = 15
        //sum = 3
        /*println(sum)*/

        Test.outter = sum + 15

        /*println(Toto.other)
        println(Test.outter)
        println(outter)
        println(a.b)*/
    }
}