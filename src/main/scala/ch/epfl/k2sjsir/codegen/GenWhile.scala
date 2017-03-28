package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.expressions.IrWhileLoop
import org.scalajs.core.ir.Trees._

case class GenWhile(d: IrWhileLoop, p: Positioner) extends Gen[IrWhileLoop] {

  def tree: Tree = {
    val cond = GenExpr(d.getCondition, p).tree
    val body = GenExpr(d.getBody, p).tree
    While(cond, body, label = None)
  }

}
