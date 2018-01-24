data class Result(val result: Int, val status: Double, val msg: String)

fun Result.test() {
    val r = Result(1,2.0, "ma string")

    println(r.component1())
    println(r.component2())
    println(r.component3())
}