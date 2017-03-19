package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.Utils._
import org.jetbrains.kotlin.descriptors.{ClassConstructorDescriptor, PropertyGetterDescriptor}
import org.jetbrains.kotlin.ir.expressions._
import org.jetbrains.kotlin.serialization.deserialization.descriptors.DeserializedSimpleFunctionDescriptor
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.{AnyType, NoType}

import scala.collection.JavaConversions._
import scala.language.implicitConversions

case class GenCall(d: IrCall, p: Positioner) extends Gen[IrCall] {

  def tree: Tree = d.getDescriptor match {
    case pd: PropertyGetterDescriptor =>
      val cp = pd.getCorrespondingProperty.getGetter
      val idt = cp.toMethodIdent
      val tpe = cp.getReturnType.toJsType
      val r = d.getDispatchReceiver
      val receiver = if (r != null) GenExpr(r, p).tree else VarRef(idt)(tpe)
      Apply(receiver, idt, Nil)(tpe)
    case cd: ClassConstructorDescriptor =>
      val tpe = cd.getContainingDeclaration.toJsClassType
      val args = cd.getValueParameters.map(_.toVarRef)
      Apply(This()(tpe), cd.toMethodIdent, args.toList)(tpe)
    case sf: DeserializedSimpleFunctionDescriptor =>
      val idt = sf.toMethodIdent
      val r = d.getDispatchReceiver
      val rec = if (r == null) This()(AnyType) else GenExpr(r, p).tree
      val args = for (i <- 0 until sf.getValueParameters.size()) yield GenExpr(d.getValueArgument(i), p).tree
      Apply(rec, idt, args.toList)(NoType)
    case _ => notImplemented
  }

}
