package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors._
import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.{AnyType, ClassType}

import scala.collection.JavaConverters._

case class GenCall(d: IrCall, p: Positioner) extends Gen[IrCall] {

  def tree: Tree = d.getDescriptor match {
    case pd: PropertyGetterDescriptor =>
      val cp = pd.getCorrespondingProperty.getGetter
      val idt = cp.toJsMethodIdent
      val tpe = cp.getReturnType.toJsType
      val r = d.getDispatchReceiver
      val receiver = if (r != null) GenExpr(r, p).tree else VarRef(idt)(tpe)
      Apply(receiver, idt, Nil)(tpe)
    case cd: ClassConstructorDescriptor =>
      val tpe = cd.getReturnType.toJsType
      val args = cd.getValueParameters.asScala.map(_.toJsVarRef)
      Apply(This()(tpe), cd.toJsMethodIdent, args.toList)(tpe)
    case sf: SimpleFunctionDescriptor =>
      val tpe = sf.getReturnType.toJsType
      val args = genArgs(sf.getValueParameters.asScala)
      if (sf.getName.toString == "println") {
        val rec = LoadModule(ClassType("s_Predef$"))
        val method = Ident("println__O__V", Some("println"))
        Apply(rec, method, args.toList)(tpe)
      } else {
        val method = sf.toJsMethodIdent
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
