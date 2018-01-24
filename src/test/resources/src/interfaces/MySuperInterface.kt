interface MySuperInterface {
    var dog: Dog

    fun buyADog(): Unit {
        dog = Dog("Rex")
        println("I bought a dog ! His name is ${dog.name}.")
    }
}