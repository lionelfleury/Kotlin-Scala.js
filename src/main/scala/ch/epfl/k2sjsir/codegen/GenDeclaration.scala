package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.declarations._
import org.jetbrains.kotlin.ir.expressions.IrStatementContainer
import org.scalajs.core.ir.Trees.Skip
import org.scalajs.core.ir.{Trees => js}

case class GenDeclaration[T <: IrDeclaration](d: T, p: Positioner)
  extends AbstractGenerator[T](d, p) {

  def generate(): js.Tree = d match {
    case f: IrFunction => GenFun(f, p).generate()
    case b: IrStatementContainer => GenBlock(b, p).generate()
    case x: IrField => GenField(x, p).generate()
    case x: IrProperty => GenProperty(x, p).generate()
    //    case _: IrAnonymousInitializer => Skip()
    //    case _: IrTypeAlias => Skip()
    //    case _: IrClass => Skip()
    case _ =>
      println(s"Not supported Declaration: $d")
      Skip()
  }

}
