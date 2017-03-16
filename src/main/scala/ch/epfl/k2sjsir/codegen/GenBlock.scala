package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.IrStatement
import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.Position
import org.scalajs.core.ir.Trees._

import scala.collection.JavaConversions._
import scala.language.implicitConversions

case class GenBlock[T <: IrStatementContainer](d: T, p: Positioner) {

  def generate()(implicit pos: Position): Tree = {
    val stats = genStats(d.getStatements.toList)
    Block(stats)
  }

  private def genStats(stats: List[IrStatement]): List[Tree] = stats.map(d => GenStat(d, p).generate())

}
