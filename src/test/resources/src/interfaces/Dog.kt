interface Animal {
    fun makeSound()
    fun beHappy()
}

class Dog(val name: String): Animal {
    fun bark() = println("Barking !")

    override fun makeSound() = bark()
    override fun beHappy() = println("Your dog is moving his tail in happiness")

    override fun toString(): String {
        return "Dog("+name+")"
    }
}