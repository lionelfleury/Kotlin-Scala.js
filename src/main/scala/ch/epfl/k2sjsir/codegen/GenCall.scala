package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.codegen.GenBinaryOp.isBinaryOp
import ch.epfl.k2sjsir.codegen.GenUnaryOp.isUnaryOp
import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors._
import org.jetbrains.kotlin.ir.descriptors.IrBuiltinOperatorDescriptorBase
import org.jetbrains.kotlin.ir.expressions._
import org.jetbrains.kotlin.resolve.DescriptorUtils
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.{ClassType, _}

import scala.collection.JavaConverters._

case class GenCall(d: IrCall, p: Positioner) extends Gen[IrCall] {

  def tree: Tree = {
    val desc = d.getDescriptor
    val name = desc.getName.toString
    val args = genArgs(desc.getValueParameters.asScala)
    desc match {
      case bt: IrBuiltinOperatorDescriptorBase =>
        args match {
          case Seq(a, b) =>
            val op = GenBinaryOp.getBinaryOp(bt.getName.toString, a.tpe)
            BinaryOp(op, a, b)
          case Seq(a) if isBooleanType(a.tpe) => UnaryOp(UnaryOp.Boolean_!, a)
          case _ => notImplemented
        }
      case cd: ClassConstructorDescriptor =>
        val tpe = cd.getConstructedClass.toJsClassType
        New(tpe, cd.toJsMethodIdent, args.toList)
      case sf: SimpleFunctionDescriptor =>
        if (name == "println" || name == "print") {
          val rec = LoadModule(ClassType("s_Predef$"))
          val method = Ident("println__O__V", Some("println"))
          val tpe = sf.getReturnType.toJsType
          Apply(rec, method, args.toList)(tpe)
        }
        else if (isBinaryOp(name)) GenBinaryOp(d, p).tree
        else if (isUnaryOp(name)) GenUnaryOp(d, p).tree
        else genApply(sf, sf.toJsMethodIdent, args.toList)
      case f: FunctionDescriptor => genApply(f, f.toJsMethodIdent, args.toList)
      case _ => notImplemented
    }
  }

  private def genApply(desc: CallableDescriptor, method: Ident, args: List[Tree]) = {
    val tpe = desc.getReturnType.toJsType
    val static = DescriptorUtils.isStaticDeclaration(desc)
    if (static) {
      val ctpe = DescriptorUtils.getContainingClass(desc).toJsClassType
      ApplyStatic(ctpe, method, args)(tpe)
    } else {
      Apply(GenExpr(d.getDispatchReceiver, p).tree, method, args)(tpe)
    }
  }

  private def genArgs(as: Seq[ValueParameterDescriptor]): Seq[Tree] =
    for (i <- as.indices) yield GenExpr(d.getValueArgument(i), p).tree

  private def isBooleanType(t: Type) = t == BooleanType

}
