package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.expressions.IrReturn
import org.scalajs.core.ir.Trees.Tree

case class GenReturn(d: IrReturn, p: Positioner) extends Gen[IrReturn] {

  def tree: Tree = {
    //TODO: See how to handle the return depending on types
    val ret = GenExpr(d.getValue, p).tree
    //    val tpe = ret.tpe
    //    if (tpe == NothingType || tpe == NoType) ret else Return(ret, label = None)
    ret
  }

}
