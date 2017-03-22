package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.expressions.IrDoWhileLoop
import org.scalajs.core.ir.Trees._

case class GenDoWhile(d: IrDoWhileLoop, p: Positioner) extends Gen[IrDoWhileLoop] {

  def tree: Tree = {
    val cond = GenExpr(d.getCondition, p).tree
    val body = GenExpr(d.getBody, p).tree
    DoWhile(body, cond, label = None)
  }

}
