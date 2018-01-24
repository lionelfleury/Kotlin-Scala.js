class Variable(val name: String, var value: Int) {
    var walkStrength: Int = 0
}

abstract class MyAbstractClass(val myOutput: Variable) {
    fun calculate() {
        println(myOutput.walkStrength)
        myOutput.walkStrength = 1
    }
}