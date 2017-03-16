package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.IrElement
import org.jetbrains.kotlin.ir.declarations._
import org.jetbrains.kotlin.ir.expressions.impl.IrBlockImpl
import org.jetbrains.kotlin.ir.expressions.{IrBlock, IrBlockBody, IrStatementContainer}
import org.scalajs.core.ir.Trees.Skip
import org.scalajs.core.ir.{Trees => js}

case class GenDeclaration[T <: IrDeclaration](d: T, p: Positioner)
  extends AbstractGenerator[T](d, p) {

  def generate(): js.Tree = d match {
    case _: IrField =>
      Skip() //GenField(d, p).generate()
    case _: IrFunction =>
      GenFun(d.asInstanceOf[IrFunction], p).generate()
    case _: IrStatementContainer =>
      GenBlock(d.asInstanceOf[IrStatementContainer], p).generate()
    case _: IrAnonymousInitializer =>
      Skip()
    case _: IrTypeAlias =>
      Skip()
    case _: IrClass =>
      Skip()
    case _ =>
      println(s"Not supported Declaration: $d")
      Skip()
  }

}
