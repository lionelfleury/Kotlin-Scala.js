package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.Trees.{Skip, Tree}

import scala.language.implicitConversions

case class GenExpr(d: IrExpression, p: Positioner) extends Gen[IrExpression] {

  def tree: Tree = d match {
    case null =>
      println("Trying to generate a null expression!!!")
      Skip()
    case g: IrGetField => GenGetField(g, p).tree
    case c: IrCall => GenCall(c, p).tree
    case c: IrConst[_] => GenConst(c, p).tree
    case g: IrGetValue => GenGetValue(g, p).tree
<<<<<<< HEAD
=======
    case b: IrBlock => GenBlock(b, p).tree
>>>>>>> 6e3217137d084c977c6d4e1b84fb90152699bc75
    case _ => notImplemented
  }

}
