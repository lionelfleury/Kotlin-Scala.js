package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.IrStatement
import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.Trees.{Block, Tree}

import scala.collection.JavaConversions._
import scala.language.implicitConversions

case class GenBlockBody(d: IrBlockBody, p: Positioner) extends Gen[IrBlockBody] {

  def tree: Tree = {
    val stats = genStats(d.getStatements.toList)
    Block(stats)
  }

  private def genStats(stats: List[IrStatement]): List[Tree] =
    stats.map(d => GenStat(d, p).tree)

}
