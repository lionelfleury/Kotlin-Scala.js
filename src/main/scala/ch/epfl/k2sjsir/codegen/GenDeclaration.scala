package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.declarations._
import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.{Trees => js}

case class GenDeclaration(d: IrDeclaration, p: Positioner) extends Gen[IrDeclaration] {

  def tree: js.Tree = d match {
    case f: IrFunction => GenFun(f, p).tree
    case b: IrBlock => GenBlock(b, p).tree
    case x: IrField => GenField(x, p).tree
    case x: IrProperty => GenProperty(x, p).tree
    case c: IrClass => GenClass(c, p).tree
    case _ => notImplemented
  }

}
