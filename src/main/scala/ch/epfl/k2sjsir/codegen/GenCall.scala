package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.codegen.GenBinaryOp.{getBinaryOp, isBinaryOp}
import ch.epfl.k2sjsir.codegen.GenUnaryOp.isUnaryOp
import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors._
import org.jetbrains.kotlin.ir.descriptors.IrBuiltinOperatorDescriptorBase
import org.jetbrains.kotlin.ir.expressions._
import org.jetbrains.kotlin.load.java.descriptors.JavaMethodDescriptor
import org.jetbrains.kotlin.resolve.DescriptorUtils._
import org.jetbrains.kotlin.serialization.deserialization.descriptors.DeserializedSimpleFunctionDescriptor
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
            val op = if(desc.getContainingDeclaration.getName.asString() == "ir") {
              GenBinaryOp.getBuiltinOp(bt.getName.toString)
            } else {
              GenBinaryOp.getBinaryOp(bt.getName.toString, a.tpe)
            }
            BinaryOp(op, a, b)
          case Seq(a) if isBooleanType(a.tpe) => UnaryOp(UnaryOp.Boolean_!, a)
          case _ => notImplemented
        }
      case cd: ConstructorDescriptor =>
        val tpe = cd.getConstructedClass.toJsClassType
        val method = cd.toJsMethodIdent
        New(tpe, method, args.toList)
      case j: JavaMethodDescriptor =>
        genApply(j, j.toJsMethodIdent, args.toList)
      case sf: DeserializedSimpleFunctionDescriptor =>
        if (name == "println" || name == "print") {
          val rec = LoadModule(ClassType("s_Predef$"))
          val method = Ident(s"${name}__O__V", Some(name))
          val tpe = sf.getReturnType.toJsType
          Apply(rec, method, args.toList)(tpe)
        }
        else if (isBinaryOp(name)) GenBinaryOp(d, p).tree
        else if (isUnaryOp(name)) GenUnaryOp(d, p).tree
        else if (name == "arrayOf") {
          args.head
        } else genApply(sf, sf.toJsMethodIdent, args.toList)
      case p: PropertyGetterDescriptor =>
        genApply(p, Ident("length__I"), args.toList)
      case _ => notImplemented
    }
  }

  private def genApply(desc: CallableDescriptor, method: Ident, args: List[Tree]) = {
    val tpe = desc.getReturnType.toJsType
    val static = isStaticDeclaration(desc)
    val x = desc.getExtensionReceiverParameter
    if (x != null) {
      val t = x.getType.toJsType
      This()(t) //TODO: Extension receiver present...
    } else if (static) {
      // TODO: Arrays class is encoded as a Module
      val name = getContainingClass(desc).toJsClassName
      if (name == null) println("Desc is null")
      Apply(LoadModule(ClassType(name + (if (name.endsWith("$")) "" else "$"))), method, args)(tpe)
    } else {
      val rec = GenExpr(d.getDispatchReceiver, p).tree
      Apply(rec, method, args)(tpe)
    }
  }

  private def genArgs(as: Seq[ValueParameterDescriptor]): Seq[Tree] =
    for (i <- as.indices) yield {
      val arg = d.getValueArgument(i)
      arg match {
        case a: IrVararg => GenVararg(a, p).tree
        case _ => GenExpr(arg, p).tree
      }
    }

  private def isBooleanType(t: Type) = t == BooleanType

}
