package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.{Trees => js}

import scala.language.implicitConversions

case class GenExpr(d: IrExpression, p: Positioner) extends Gen[IrExpression] {

  def tree: js.Tree = d match {
    case g: IrGetField => GenGetField(g, p).tree
    case c: IrCall => GenCall(c, p).tree
    case c: IrConst[_] => GenConst(c, p).tree
    case _ => notImplemented
  }

}
