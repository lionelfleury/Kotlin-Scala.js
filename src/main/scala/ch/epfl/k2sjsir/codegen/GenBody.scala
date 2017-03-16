package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.expressions.{IrBody, IrStatementContainer}
import org.scalajs.core.ir.Trees._

import scala.language.implicitConversions

case class GenBody[T <: IrBody](d: T, p: Positioner) extends AbstractGenerator[T](d, p) {

  def generate(): Tree = {
    implicit val pos = p.getPos(d.getStartOffset)
    GenBlock(d.asInstanceOf[IrStatementContainer], p).generate()
  }

}
