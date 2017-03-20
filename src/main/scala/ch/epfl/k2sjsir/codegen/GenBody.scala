package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.expressions.{IrBlock, IrBlockBody, IrBody}
import org.scalajs.core.ir.Trees._

import scala.language.implicitConversions

case class GenBody(d: IrBody, p: Positioner) extends Gen[IrBody] {

  def tree: Tree = d match {
    case null => Block(Nil)
    case b: IrBlock => GenBlock(b, p).tree
    case b: IrBlockBody => GenBlockBody(b, p).tree
    case _ => notImplemented
  }

  def treeOption: Option[Tree] = if (d == null) None else Some(tree)

}
