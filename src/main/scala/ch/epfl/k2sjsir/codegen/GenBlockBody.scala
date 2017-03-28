package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.Trees.{Block, Tree}

import scala.collection.JavaConverters._

case class GenBlockBody(d: IrBlockBody, p: Positioner) extends Gen[IrBlockBody] {

  def tree: Tree = d.getStatements match {
    case null => Block(Nil)
    case stats => Block(stats.asScala.map(GenStat(_, p).tree).toList)
  }

}
