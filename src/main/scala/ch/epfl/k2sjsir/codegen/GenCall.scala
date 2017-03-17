package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.NameEncoder
import org.jetbrains.kotlin.descriptors.{ClassDescriptor, FunctionDescriptor}
import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.{Trees => js}

import scala.language.implicitConversions

case class GenCall(d: IrCall, p: Positioner) extends Gen[IrCall] {

  def tree: js.Tree = {
    val descriptor = d.getDescriptor
    val tpe = descriptor.getReturnType.toJsType
    val name = js.Ident(NameEncoder.encodeName(descriptor.getName.asString()))
    val receiver = js.This()(tpe)
    //TODO: Parameter list
    js.Apply(receiver, name, Nil)(tpe)
  }

}
