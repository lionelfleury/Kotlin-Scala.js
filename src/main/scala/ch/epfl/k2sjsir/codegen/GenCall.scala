package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.Utils._
import org.jetbrains.kotlin.descriptors.{ClassConstructorDescriptor, PropertyGetterDescriptor, SimpleFunctionDescriptor}
import org.jetbrains.kotlin.ir.expressions._
import org.jetbrains.kotlin.serialization.deserialization.descriptors.DeserializedSimpleFunctionDescriptor
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.{ClassType, NoType}

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
      //TODO: Hack to get a Hello, World!
      val rec = LoadModule(ClassType("s_Predef$"))
      val idt = sf.toMethodIdent
      val args = for (i <- 0 until sf.getValueParameters.size()) yield GenExpr(d.getValueArgument(i), p).tree
      Apply(rec, idt, args.toList)(d.getType.toJsType)
    case f: SimpleFunctionDescriptor =>
      val r = f.getDispatchReceiverParameter
      val args = f.getValueParameters.map(_.toVarRef)
      val tpe = ClassType("sjs_js_JSApp")
      val idt = Ident("$$js$exported$meth$main__O")
      val receiver = This()(NoType)
      ApplyStatically(receiver, tpe, idt, args.toList)(NoType)
    case _ => notImplemented
  }

}
