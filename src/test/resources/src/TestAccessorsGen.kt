
class MyClassWithAccessors {

    var myVar: Int = 0
        get
        set(value) {
            if (value >= 2)
                field = value - 1
            else
                field = -1
        }

    val name: String
        get() = "name"

    val isEmpty: Boolean
        get() {
            if (this.myVar >= 0)
                return true
            else
                return false
        }

    val id: Int = 1

    var tmp: String = "tmp"
        set(value) {
            field = value + "_set"
        }

    var customSet: String = "customSet"
        set(myVal) {
            field = myVal + "_set"
        }

    var nodefault: Int
        get() = 2
        set(value: Int) {}
}

object TestAccessorsGen {
    fun main(args: Array<String>) {
        val a = MyClassWithAccessors()

        println(a.id)
        println(a.name)
        println(a.myVar)
        a.myVar = 4
        println(a.myVar)
        a.myVar = 0
        println(a.myVar)
        println(a.tmp)
        a.tmp = "tmp2"
        println(a.tmp)
        println(a.customSet)
        a.customSet = "not_using_default_parameter_name"
        println(a.customSet)

    }
}