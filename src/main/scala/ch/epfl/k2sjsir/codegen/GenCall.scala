package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors._
import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.ClassType

import scala.collection.JavaConverters._

case class GenCall(d: IrCall, p: Positioner) extends Gen[IrCall] {

  def tree: Tree = {
    val f = d.getDescriptor
    val idt =
      if (f.getName.toString == "println") Ident("println__O__V", Some("println"))
      else f.toJsMethodIdent
    val args = genArgs(f.getValueParameters.asScala)
    val tpe = f.getReturnType.toJsType
    val rec =
      if (f.getName.toString == "println") LoadModule(ClassType("s_Predef$"))
      else GenExpr(d.getDispatchReceiver, p).tree
    Apply(rec, idt, args.toList)(tpe)
  }

  private def genArgs(as: Seq[ValueParameterDescriptor]): Seq[Tree] =
    for (i <- as.indices) yield GenExpr(d.getValueArgument(i), p).tree

}
