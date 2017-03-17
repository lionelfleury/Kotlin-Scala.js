package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.expressions.IrReturn
import org.scalajs.core.ir.{Trees => js}

case class GenReturn(d: IrReturn, p: Positioner) extends Gen[IrReturn] {

  def tree: js.Tree = {
    val e = d.getValue
    val label = None
    val expr = GenExpr(e, p).tree
    js.Return(expr, label = label)
  }

}
