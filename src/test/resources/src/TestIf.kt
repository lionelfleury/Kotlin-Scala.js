fun main(args: Array<String>) {
    val a = 1

    /* Without else */
    if(a == 1) println(a)

    /* With else */
    if(a == 2) println("error")
    else println(a)

    /* Else if */
    if(a == 2) println("error")
    else if(a == 1) println(1)
    else println("error")
}