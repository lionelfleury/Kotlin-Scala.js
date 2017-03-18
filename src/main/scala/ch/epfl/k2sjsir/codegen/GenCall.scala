package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.Utils._
import org.jetbrains.kotlin.descriptors.{ClassConstructorDescriptor, PropertyGetterDescriptor}
import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.NoType

import scala.collection.JavaConversions._
import scala.language.implicitConversions

case class GenCall(d: IrCall, p: Positioner) extends Gen[IrCall] {

  def tree: Tree = d.getDescriptor match {
    case pd: PropertyGetterDescriptor =>
      val idt = pd.toIdent
      val tpe = pd.getReturnType.toJsType
      val r = d.getDispatchReceiver
      val receiver = if (r != null) GenExpr(r, p).tree else VarRef(idt)(tpe)
      Apply(receiver, idt, Nil)(tpe)
    case cd: ClassConstructorDescriptor =>
      val tpe = cd.getContainingDeclaration.toJsClassType
      val args = cd.getValueParameters.map(_.toVarRef)
      ApplyStatically(This()(NoType), tpe, cd.toIdent, args.toList)(tpe)
    case _ => notImplemented
  }

}
