object TestMethodsBaseTypes {
    fun main(args: Array<String>) {
        run()
    }
}

fun run() {

    // From Kotlin docs
    val oneMillion = 1_000_000
    val creditCardNumber = 1234_5678_9012_3456L
    val socialSecurityNumber = 999_99_9999L
    val hexBytes = 0xFF_EC_DE_5E
    val bytes = 0b11010010_01101001_10010100_10010010

    println(oneMillion)
    println(creditCardNumber)
    println(socialSecurityNumber)
    println(hexBytes)
    println(bytes)

    //FIXME: naming a variable "long" leads to translation errors ?
    // Conversion from Int to ...
    println("=== Type cast : from Int to X ===")

    val myInt : Int = 65
    val myIntAsByte = myInt.toByte()
    val myIntAsShort = myInt.toShort()
    val myIntAsLong = myInt.toLong()
    val myIntAsFloat = myInt.toFloat()
    val myIntAsDouble = myInt.toDouble()
    val myIntAsChar = myInt.toChar()

    println("Int $myInt")
    println("Byte $myIntAsByte")
    println("Short $myIntAsShort")
    println("Long $myIntAsLong")
    println("Float $myIntAsFloat")
    println("Double $myIntAsDouble")
    println("Char $myIntAsChar")

    // Conversion from Byte to ...
    println("=== Type cast : from Byte to X ===")

    val myByte : Byte = 0b0100_0001
    val myByteAsInt = myByte.toInt()
    val myByteAsShort = myByte.toShort()
    val myByteAsLong = myByte.toLong()
    val myByteAsFloat = myByte.toFloat()
    val myByteAsDouble = myByte.toDouble()
    val myByteAsChar = myByte.toChar()

    println("Byte $myByte")
    println("Int $myByteAsInt")
    println("Short $myByteAsShort")
    println("Long $myByteAsLong")
    println("Float $myByteAsFloat")
    println("Double $myByteAsDouble")
    println("Char $myByteAsChar")

    // Conversion from Short to ...
    println("=== Type cast : from Short to X ===")

    val myShort : Short = 65
    val myShortAsInt = myShort.toInt()
    val myShortAsByte = myShort.toByte()
    val myShortAsLong = myShort.toLong()
    val myShortAsFloat = myShort.toFloat()
    val myShortAsDouble = myShort.toDouble()
    val myShortAsChar = myShort.toChar()

    println("Short $myShort")
    println("Int $myShortAsInt")
    println("Byte $myShortAsByte")
    println("Long $myShortAsLong")
    println("Float $myShortAsFloat")
    println("Double $myShortAsDouble")
    println("Char $myShortAsChar")

    // Conversion from Long to ...
    println("=== Type cast : from Long to X ===")

    val myLong : Long = 65
    val myLongAsInt = myLong.toInt()
    val myLongAsByte = myLong.toByte()
    val myLongAsShort = myLong.toShort()
    val myLongAsFloat = myLong.toFloat()
    val myLongAsDouble = myLong.toDouble()
    val myLongAsChar = myLong.toChar()

    println("Long $myLong")
    println("Int $myLongAsInt")
    println("Byte $myLongAsByte")
    println("Short $myLongAsShort")
    println("Float $myLongAsFloat")
    println("Double $myLongAsDouble")
    println("Char $myLongAsChar")

    // Conversion from Float to ...
    println("=== Type cast : from Float to X ===")

    val myFloat : Float = 65.4f
    val myFloatAsInt = myFloat.toInt()
    val myFloatAsByte = myFloat.toByte()
    val myFloatAsShort = myFloat.toShort()
    val myFloatAsLong = myFloat.toLong()
    val myFloatAsDouble = myFloat.toDouble()
    val myFloatAsChar = myFloat.toChar()

    println("Float $myFloat")
    println("Int $myFloatAsInt")
    println("Byte $myFloatAsByte")
    println("Short $myFloatAsShort")
    println("Long $myFloatAsLong")
    println("Double $myFloatAsDouble")
    println("Char $myFloatAsChar")

    // Conversion from Double to ...
    println("=== Type cast : from Double to X ===")

    val myDouble : Double = 65.7
    val myDoubleAsInt = myDouble.toInt()
    val myDoubleAsByte = myDouble.toByte()
    val myDoubleAsShort = myDouble.toShort()
    val myDoubleAsFloat = myDouble.toFloat()
    val myDoubleAsLong = myDouble.toLong()
    val myDoubleAsChar = myDouble.toChar()

    println("Double $myDouble")
    println("Int $myDoubleAsInt")
    println("Byte $myDoubleAsByte")
    println("Short $myDoubleAsShort")
    println("Float $myDoubleAsFloat")
    println("Long $myDoubleAsLong")
    println("Char $myDoubleAsChar")

    // Conversion from Char to ...
    println("=== Type cast : from Char to X ===")

    val myChar : Char = 'A'
    val myCharAsInt = myChar.toInt()
    val myCharAsByte = myChar.toByte()
    val myCharAsShort = myChar.toShort()
    val myCharAsFloat = myChar.toFloat()
    val myCharAsLong = myChar.toLong()
    val myCharAsDouble = myChar.toDouble()

    println("Char $myChar")
    println("Int $myCharAsInt")
    println("Byte $myCharAsByte")
    println("Short $myCharAsShort")
    println("Float $myCharAsFloat")
    println("Long $myCharAsLong")
    println("Double $myCharAsDouble")
}