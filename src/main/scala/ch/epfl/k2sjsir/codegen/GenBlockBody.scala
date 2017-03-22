package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.Trees.{Block, Tree}

import scala.collection.JavaConverters._

case class GenBlockBody(d: IrBlockBody, p: Positioner) extends Gen[IrBlockBody] {

  def tree: Tree = {
    val stats = d.getStatements.asScala.map(GenStat(_, p).tree)
    Block(stats.toList)
  }

}
