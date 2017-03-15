package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.IrStatement
import org.jetbrains.kotlin.ir.expressions.{IrBlockBody, IrBody}
import org.scalajs.core.ir.Trees._

import scala.collection.JavaConversions._
import scala.language.implicitConversions

case class GenBody(d: IrBody, p: Positioner) extends AbstractGenerator(d, p) {

  def generate(): Tree = if (d == null) Block(Nil)
  else d match {
    case ds: IrBlockBody =>
      Block(ds.asInstanceOf[IrBlockBody].getStatements.map(genStat).toList)
    case _ => println(s"Not supported $d")
      Skip()
  }

  private def genStat(d: IrStatement): Tree = {
    Skip()
  }

}
