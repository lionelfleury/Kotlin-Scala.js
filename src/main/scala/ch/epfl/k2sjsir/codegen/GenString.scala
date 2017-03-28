package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.Trees.{Skip, Tree}

case class GenString(d: IrStringConcatenation, p: Positioner) extends Gen[IrStringConcatenation] {

  def tree: Tree = {
    Skip()
  }

}
