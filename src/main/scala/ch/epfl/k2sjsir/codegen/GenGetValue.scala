package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.NameEncoder
import ch.epfl.k2sjsir.Utils._
import org.jetbrains.kotlin.ir.expressions.IrGetValue
import org.scalajs.core.ir.{Trees => js}

case class GenGetValue(d: IrGetValue, p: Positioner) extends Gen[IrGetValue] {

  def tree: js.Tree = {
    val desc = d.getDescriptor
    val name = NameEncoder.encodeName(desc.getName.asString())
    val tpe = desc.getReturnType.toJsType
    js.VarRef(js.Ident(name))(tpe)
  }

}
