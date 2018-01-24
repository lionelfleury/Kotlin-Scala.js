/*fun String.count(): Int {
    return length
}*/

fun String.count() = length
fun String.manyCount(other: String): Int {
    return count() + other.count()
}

object TopClassExtensionTester {


    fun main() {
        val content = "Toto"
        val otherContent = "Titito"


        val sumLength = content.manyCount(otherContent)
        println(sumLength)
    }
}

object TestTopClassExtension {
    fun main(args: Array<String>) {
        TopClassExtensionTester.main()
    }
}