
object TestDom {
    fun main(args: Array<String>) {
        document.title = "Hello World"
        // val body = document.body
        //document.body.innerHTML = "Hello World"
    }
}

@JsName("document")
external class document {

    companion object {
        var title: String
        val body: Element
        fun getElementById(id: String): Element
        fun createElement(tag: String): Element
    }
}

external class Element {
    companion object {
        var textContent: String
        var innerHTML: String

        fun appendChild(child: Element): Unit
        var onclick: () -> Unit
    }
}