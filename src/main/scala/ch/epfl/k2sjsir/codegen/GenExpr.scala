package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.expressions.{IrExpression, IrGetField, IrGetValue}
import org.scalajs.core.ir.Trees.Skip
import org.scalajs.core.ir.{Trees => js}

case class GenExpr(d: IrExpression, p: Positioner) extends Gen[IrExpression] {

  def tree: js.Tree = d match {
    case g: IrGetField => GenGetField(g, p).tree
    case g: IrGetValue =>
      val r = g.getDescriptor
      val tpe = r.getType.toJsType
      val name = r.getName.asString()
      if (name == "<this>") js.This()(tpe)
      else {
        println(s"Not supported GetValue in Expr: $d")
        js.This()(tpe)
      }
    //GenGetValue(g, p).generate()
    case _ =>
      println(s"Not supported Expr: $d")
      Skip()
  }

}
