package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.Utils._
import org.jetbrains.kotlin.descriptors.ValueParameterDescriptor
import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.Trees._

import scala.collection.JavaConversions._
import scala.language.implicitConversions

case class GenCall(d: IrCall, p: Positioner) extends Gen[IrCall] {

  def tree: Tree = {
    val desc = d.getDescriptor
    val tpe = desc.getReturnType.toJsType
    val name = Ident(desc.getName.toJsName)
    val r = d.getDispatchReceiver
    val receiver = if (r != null) GenExpr(r, p).tree else This()(tpe)
    val args = for (a: ValueParameterDescriptor <- desc.getValueParameters) yield {
      val name = a.getName.toJsName
      val tpe = a.getType.toJsType
      VarRef(Ident(name))(tpe)
    }
    Apply(receiver, name, args.toList)(tpe)
  }

}
