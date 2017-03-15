package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.declarations._
import org.scalajs.core.ir.Trees.Skip
import org.scalajs.core.ir.{Trees => js}

case class GenDeclaration(irDeclaration: IrDeclaration, p: Positioner)
  extends AbstractGenerator(irDeclaration, p) {

  def generate(): js.Tree = irDeclaration match {
    case _: IrField =>
      Skip() //GenField(d, p).generate()
    case _: IrFunction => GenFun(irDeclaration.asInstanceOf[IrFunction], p).generate()
    case _: IrAnonymousInitializer =>
      Skip()
    case _: IrTypeAlias =>
      Skip()
    case _: IrClass =>
      Skip()
    case _ =>
      println(s"Not supported $irDeclaration")
      Skip()
  }

}
