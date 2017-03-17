package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.NameEncoder
import org.jetbrains.kotlin.ir.expressions.IrGetField
import org.scalajs.core.ir.{Trees => js}

case class GenGetField(d: IrGetField, p: Positioner) extends Gen[IrGetField] {

  def tree: js.Tree = {
    val g = d.getDescriptor.getGetter
    val c = d.getSuperQualifier
    val method = NameEncoder.encodeMethodIdent(g, c)
    val r = d.getReceiver
    val receiver = GenExpr(r, p).tree
    js.Apply(receiver, method, Nil)(receiver.tpe)
  }

}
