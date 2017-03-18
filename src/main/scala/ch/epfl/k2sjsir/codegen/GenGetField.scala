package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.NameEncoder
import ch.epfl.k2sjsir.Utils._
import org.jetbrains.kotlin.ir.expressions.IrGetField
import org.scalajs.core.ir.{Trees => js}

case class GenGetField(d: IrGetField, p: Positioner) extends Gen[IrGetField] {

  def tree: js.Tree = {
    val g = d.getDescriptor
    val name = NameEncoder.encodeName(g.getName.asString())
    val tpe = g.getReturnType.toJsType
    js.VarRef(js.Ident(name))(tpe)
  }

}
