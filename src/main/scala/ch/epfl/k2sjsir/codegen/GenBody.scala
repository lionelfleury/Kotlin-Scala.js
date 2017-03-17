package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.expressions.{IrBlockBody, IrBody}
import org.scalajs.core.ir.Trees._

import scala.language.implicitConversions

case class GenBody(d: IrBody, p: Positioner) extends Gen[IrBody] {

  def tree: Tree = d match {
    case b: IrBlockBody => GenBlock(b, p).tree
    case _ =>
      println(s"Not supported $getClass: $d")
      Skip()
  }

}
