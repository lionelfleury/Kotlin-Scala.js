package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.Utils._
import org.jetbrains.kotlin.descriptors._
import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.{AnyType, ClassType}

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
    case sf: SimpleFunctionDescriptor =>
      val tpe = sf.getReturnType.toJsType
      val args = genArgs(sf.getValueParameters)
      if (sf.getName.toString == "println") {
        val rec = LoadModule(ClassType("s_Predef$"))
        val method = Ident("println__O__V", Some("println"))
        Apply(rec, method, args.toList)(tpe)
      } else {
        val method = sf.toMethodIdent
        val r = d.getDispatchReceiver
        if (r == null) println("Null receiver in GenCall!!")
        val rec = if (r == null) This()(AnyType) else GenExpr(r, p).tree
        Apply(rec, method, args.toList)(tpe)
      }
    case _ => notImplemented
  }

  private def genArgs(as: Seq[ValueParameterDescriptor]): Seq[Tree] =
    for (i <- as.indices) yield GenExpr(d.getValueArgument(i), p).tree

}
