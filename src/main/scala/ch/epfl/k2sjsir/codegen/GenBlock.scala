package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.Trees.{Block, Tree}

import scala.collection.JavaConversions._
import scala.language.implicitConversions

case class GenBlock(d: IrBlock, p: Positioner) extends Gen[IrBlock] {

  def tree: Tree = {
    val stats = d.getStatements.map(GenStat(_, p).tree)
    Block(stats.toList)
  }

}
