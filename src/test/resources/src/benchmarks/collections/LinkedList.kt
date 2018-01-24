package benchmarks.collections

interface MyMutableList<E> {

    val size: Int

    val lastIndex: Int

    fun isEmpty(): Boolean

    fun contains(element: E): Boolean

    fun add(element: E): Boolean

    fun remove(element: E): Boolean

    fun removeAt(index: Int): E

    fun get(index: Int): E

    fun indexOf(element: E): Int

    fun <F>map(p: (E) -> F): LinkedList<F>

    fun joinToString(separator: String): String

    fun iterator(): LinkedListIterator<E>
}

class Node<E>(var value: E, var next: Node<E>?) {
    override fun toString(): String {
        return "Node("+ value +")"
    }
}

class LinkedListIterator<E>(val head: Node<E>?) {
    var currentNode = head

    fun hasNext(): Boolean {
        return currentNode != null
    }

    fun next(): E {
        if (hasNext()) {
            val ret = currentNode!!.value
            currentNode = currentNode!!.next
            return ret
        } else {
            throw Exception("Reached end of iterator")
        }
    }
}

class LinkedList<E>(): MyMutableList<E> {
    var head: Node<E>? = null
    var last: Node<E>? = null

    override val size: Int
        get() {
            return _length()
        }

    override val lastIndex: Int
        get() {
            return _length() - 1
        }

    constructor(element: E): this() {
        head = Node(element, null)
        last = head
    }

    constructor(values: Array<E>): this() {
        for (v in values) {
            add(v)
        }
    }

    override fun isEmpty(): Boolean = head == null

    override fun contains(element: E): Boolean = indexOf(element) >= 0

    override fun add(element: E): Boolean {
        val newNode = Node(element, null)

        if (last != null) last!!.next = newNode
        else head = newNode

        last = newNode

        return true
    }

    fun addFirst(element: E): Boolean {
        val newNode = Node(element, head)
        head = newNode

        if (last == null) last = newNode

        return true
    }

    override fun remove(element: E): Boolean {
        var node = head
        var prev: Node<E>? = null

        while(node != null) {
            val next = node.next
            if (node.value == element) {
                if (prev == null)
                    head = next
                else
                    prev.next = next

                if (next == null)
                    last = prev

                return true
            }
            prev = node
            node = next
        }

        return false
    }

    fun removeFirst(): E {
        val node = head!!
        head = node.next

        if (head == null) last = null

        return node.value
    }

    override fun removeAt(index: Int): E {
        var node = head
        var prev: Node<E>? = null
        var i = 0
        while(node != null) {
            val next = node.next

            if (i == index) {
                if (prev == null)
                    head = next
                else
                    prev.next = next

                if (next == null)
                    last = prev

                return node.value
            }

            prev = node
            node = next
            i += 1
        }

        throw Exception("Index out of bounds")
    }

    override fun get(index: Int): E {
        var node: Node<E>? = head
        var i = 0

        while (node != null) {
            if (index == i) {
                return node.value
            }

            node = node.next
            i += 1
        }

        throw Exception("Index out of bounds")
    }

    override fun indexOf(element: E): Int {
        var node = head
        var index = 0
        while (node != null) {
            if (node.value == element)
                return index

            index += 1
            node = node.next
        }

        return -1
    }

    override fun <F> map(p: (E) -> F): LinkedList<F> {
        val newList = LinkedList<F>()

        var node = head
        while (node != null) {
            newList.add(p(node.value))

            node = node.next
        }

        return newList
    }

    override fun joinToString(separator: String): String {
        var ret = ""

        var node = head
        var first = true
        while (node != null) {
            if (first) first = false
            else ret += separator

            ret += node.value


            node = node.next
        }

        return ret
    }

    override fun iterator(): LinkedListIterator<E> {
        return LinkedListIterator<E>(head)
    }

    private fun _length(): Int {
        var currentNode = head
        var l = 0
        while (currentNode != null) {
            l += 1
            currentNode = currentNode.next
        }
        return l;
    }

    override fun toString(): String {
        return "LinkedList(" + joinToString(",") + ")"
    }
}

fun LinkedList<Int>.sum(): Int {
    var currentNode = head
    var sum = 0
    while (currentNode != null) {
        sum += currentNode.value
        currentNode = currentNode.next
    }

    return sum
}

fun LinkedList<Double>.sum(): Double {
    var currentNode = head
    var sum = 0.0
    while (currentNode != null) {
        sum += currentNode.value
        currentNode = currentNode.next
    }

    return sum
}

class MyListEntry(val value: Int) {
    override fun toString(): String {
        return "MyListEntry(value=$value)"
    }
}

object LinkedListRunner {
    fun main(args: Array<String>) {
        val l = LinkedList<Int>()

        for (i in 0 .. 10) {
            l.add(i)
        }
        println(l)

        println(l.map {
            println("mapping $it")
            it * 2
        })

        l.remove(5)
        l.removeAt(0)
        l.removeAt(6)

        println(l)

        println(l.sum())
        println("" + l.contains(3) + " " + l.contains(42))

        val l2 = LinkedList<MyListEntry>()
        val e1 = MyListEntry(3)
        val e2 = MyListEntry(95)

        l2.add(e1)
        l2.add(e2)
        println(l2)
        l2.remove(e1)
        println(l2)

        val a = arrayOf("a", "b", "c")
        val l3 = LinkedList<String>(a)

        val j = l3.iterator()

        while (j.hasNext()) {
            val e: Any = j.next()
            println(e.toString())
        }
    }
}