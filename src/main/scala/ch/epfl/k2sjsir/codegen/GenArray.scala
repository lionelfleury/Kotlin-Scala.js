package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.Trees.{Ident, New, Tree}
import org.scalajs.core.ir.Types.{ArrayType, ClassType}

case class GenArray(d: IrCall, p: Positioner, varg: Tree) extends Gen[IrCall] {

  import GenArray.arrayType

  //TODO: add other kind: intArrayOf, charArrayOf,...
  def tree: Tree = {
    val (tpe, init) = varg.tpe match {
      case ArrayType(a@"O", _) => (arrayType(a), a)
      case ArrayType(i@"I", _) => (arrayType(i), i)
      case _ => notImplemented; ""
    }
    New(ClassType(s"Lkotlin_$tpe"), Ident(s"init___A$init"), List(varg))
  }

}

object GenArray {

  private val arrayType = Map(
    "O" -> "Array",
    "I" -> "ArrayInt"
  )

  private val arrayOps = Set("arrayOf", "intArrayOf")

  def isArrayOp(name: String): Boolean = arrayOps(name)

}
