package benchmarks.collections

class Stack<T> {
    private val stack = LinkedList<T>()

    fun pop(): T {
        return stack.removeFirst()
    }

    fun push(elem: T): Stack<T> {
        stack.addFirst(elem)

        return this
    }

    fun isEmpty(): Boolean {
        return stack.isEmpty()
    }
}
