package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.expressions.IrReturn
import org.scalajs.core.ir.Trees.{Return, Tree}

case class GenReturn(d: IrReturn, p: Positioner) extends Gen[IrReturn] {

  def tree: Tree = {
    Return(GenExpr(d.getValue, p).tree, label = None)
  }

}
