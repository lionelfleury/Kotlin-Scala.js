package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.NameEncoder
import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.{Trees => js}

case class GenSetField(d: IrSetField, p: Positioner) extends Gen[IrSetField] {

  def tree: js.Tree = {
    val descriptor = d.getDescriptor
    val tpe = descriptor.getType.toJsType
    val name = NameEncoder.encodeName(descriptor.getName.asString())
    val lhs = js.VarRef(js.Ident(name))(tpe)
    val rhs = GenExpr(d.getValue, p).tree
    js.Assign(lhs, rhs)
  }

}
