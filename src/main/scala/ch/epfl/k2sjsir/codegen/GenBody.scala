package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.IrStatement
import org.jetbrains.kotlin.ir.expressions.{IrBlock, IrBlockBody, IrBody, IrDelegatingConstructorCall}
import org.scalajs.core.ir.Trees._

import scala.collection.JavaConversions._
import scala.language.implicitConversions

case class GenBody(d: IrBody, p: Positioner) extends AbstractGenerator(d, p) {

  def generate(): Tree = if (d == null) Block(Nil)
  else d match {
    case b: IrBlockBody => Block(genStats(b.getStatements.toList))
    case b: IrBlock => Block(genStats(b.getStatements.toList))
    case _ => println(s"Not supported: $d")
      Skip()
  }

  private def genStat(d: IrStatement): Tree = d match {
    case _: IrDelegatingConstructorCall =>
      //TODO: Call to super(...)
      Block(Skip(), Skip())
    case _ => println(s"Not supported $d")
      Skip()
  }

  private def genStats(stats: List[IrStatement]): List[Tree] = stats.map(genStat)

}
