package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.Trees.{Skip, Tree}

case class GenExpr(d: IrExpression, p: Positioner) extends Gen[IrExpression] {

  def tree: Tree = d match {
    case null =>
      println("Trying to generate a null expression!!!")
      Skip()
    case g: IrGetField => GenGetField(g, p).tree
    case c: IrCall => GenCall(c, p).tree
    case c: IrConst[_] => GenConst(c, p).tree
    case g: IrGetValue => GenGetValue(g, p).tree
    case b: IrBlock => GenBlock(b, p).tree
    case _ => notImplemented
  }

}
