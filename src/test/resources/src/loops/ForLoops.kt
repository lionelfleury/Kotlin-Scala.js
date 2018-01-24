package loops

class ForLoopTester {
    fun testForOverRange() {
        var sum = 0
        for (i in 0..10)
            sum += i

        println(sum)

        sum = 0
        for (j in 0..10 step 2)
            sum += j

        println(sum)
    }

    fun testForOverRangeTwice() {
        // FIXME: This generates two vardefs for i !
        // In ScalaJS the second VarDef has name i$2 which corrects the problem
        /*
        var sum = 0
        for (i in 0..10)
            sum += i

        for (i in 0..10)
            sum += i

        println(sum)
        */
    }

    fun testForOverRangeWithUntil() {

        var sum = 0
        for (i in 0 until 10)
            sum += i

        println(sum)

        sum = 0
        for (j in 0 until 10 step 2)
            sum += j

        println(sum)
    }

    fun testForOverRangeWithDownTo() {
        var sum = 0
        for (i in 10 downTo 0) {
            sum += i
        }

        println(sum)


        sum = 0
        for (j in 20 downTo 1 step 5)
            sum++

        println(sum)
    }

    fun testAll() {
        testForOverRange()
        testForOverRangeTwice()
        testForOverRangeWithUntil()
        testForOverRangeWithDownTo()
    }
}