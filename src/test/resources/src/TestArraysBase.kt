object TestArraysBase {
    fun main(args: Array<String>) {
        val a = arrayOf(1, 2, 3, 12.5)
        println(a.get(0))
        println(a.get(3))
        println(a.size)

        a[0] = 3

        println(a[0])

        val numbers = intArrayOf(1, 2, 3, 4)
        /*
        val iterOnNumbers = numbers.iterator()
        while(iterOnNumbers.hasNext()) println(iterOnNumbers.next())
        */

        val doubles = doubleArrayOf(1.2, 1.2, 1.3)
        /*
        val iterOnDoubles = doubles.iterator()
        while(iterOnDoubles.hasNext()) println(iterOnDoubles.next())
        */

        val longs = longArrayOf(124387123601, 12438877123602, 12438712300603)
        /*
        val iterOnLongs = longs.iterator()
        while(iterOnLongs.hasNext()) println(iterOnLongs.next())
        */

        val strings = arrayOf("hey", "there", "!")
        /*
        val iterOnStrings = strings.iterator()
        while(iterOnStrings.hasNext()) println(iterOnStrings.next())
        */
    }
}