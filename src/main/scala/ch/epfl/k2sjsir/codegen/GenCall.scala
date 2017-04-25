package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.codegen.GenArrayOps._
import ch.epfl.k2sjsir.codegen.GenBinaryOp.{getBinaryOp, isBinaryOp}
import ch.epfl.k2sjsir.codegen.GenUnaryOp.isUnaryOp
import ch.epfl.k2sjsir.utils.NameEncoder
import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.builtins.BuiltInsPackageFragment
import org.jetbrains.kotlin.builtins.functions.FunctionClassDescriptor
import org.jetbrains.kotlin.descriptors._
import org.jetbrains.kotlin.ir.descriptors.IrBuiltinOperatorDescriptorBase
import org.jetbrains.kotlin.ir.expressions._
import org.jetbrains.kotlin.load.java.`lazy`.descriptors.LazyJavaPackageFragment
import org.jetbrains.kotlin.load.java.descriptors.JavaMethodDescriptor
import org.jetbrains.kotlin.resolve.DescriptorUtils
import org.jetbrains.kotlin.resolve.DescriptorUtils._
import org.jetbrains.kotlin.resolve.`lazy`.descriptors.LazyPackageDescriptor
import org.jetbrains.kotlin.serialization.deserialization.descriptors.DeserializedSimpleFunctionDescriptor
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types._

import scala.collection.JavaConverters._
import scala.util.Try

case class GenCall(d: IrCall, p: Positioner) extends Gen[IrCall] {

  def tree: Tree = {
    val desc = d.getDescriptor
    val name = desc.getName.toString
    val x = Option(d.getExtensionReceiver).map(GenExpr(_, p).tree)
    val args = x ++ genArgs(desc.getValueParameters.asScala.indices)
    desc match {
      case _: IrBuiltinOperatorDescriptorBase => args match {
        case Seq(b) if name == "rem" =>
          val a = GenExpr(d.getDispatchReceiver, p).tree
          BinaryOp(getBinaryOp(name, a.tpe), a, b)
        case Seq(a, b) => BinaryOp(getBinaryOp(name, a.tpe), a, b)
        case Seq(a) if name == "LT0" => BinaryOp(BinaryOp.Num_<, a, IntLiteral(0))
        case Seq(a) if name == "LTEQ0" => BinaryOp(BinaryOp.Num_<=, a, IntLiteral(0))
        case Seq(a) if name == "GT0" => BinaryOp(BinaryOp.Num_>, a, IntLiteral(0))
        case Seq(a) if name == "GTEQ0" => BinaryOp(BinaryOp.Num_>=, a, IntLiteral(0))
        case Seq(a) if name == "NOT" => UnaryOp(UnaryOp.Boolean_!, a)
        case _ => notImplemented
      }
      case cd: ConstructorDescriptor =>
        val tpe = cd.getConstructedClass.toJsClassType
        val method = cd.toJsMethodIdent
        New(tpe, method, args.toList)
      case sf: DeserializedSimpleFunctionDescriptor =>
        if (name == "println" || name == "print") {
          val rec = LoadModule(ClassType("s_Predef$"))
          val method = Ident(s"${name}__O__V", Some(name))
          val tpe = sf.getReturnType.toJsType
          Apply(rec, method, args.toList)(tpe)
        }
        else if (isBinaryOp(name)) GenBinaryOp(d, p).tree
        else if (isUnaryOp(name)) GenUnaryOp(d, p).tree
        else genApply(sf, args.toList)
      case sf: SimpleFunctionDescriptor if isTopLevelExtension(sf) => genClassExtension(sf, args.toList)
      case _: JavaMethodDescriptor |
           _: PropertyGetterDescriptor |
           _: PropertySetterDescriptor |
           _: SimpleFunctionDescriptor => genApply(desc, args.toList)
      case _ => notImplemented
    }
  }

  private def genApply(desc: CallableDescriptor, args: List[Tree]) = {
    val method = desc.toJsMethodIdent
    val tpe = desc.getReturnType.toJsType
    val static = isStaticDeclaration(desc)
    if (isArrayOps(d)) GenArrayOps(d, p, args).tree
    else if (static) {
      val n = desc.getContainingDeclaration match {
        case _: BuiltInsPackageFragment | _: LazyJavaPackageFragment =>
          getClassDescriptorForType(desc.getReturnType).toJsClassName
        case c: ClassDescriptor => c.toJsClassName
        case p: LazyPackageDescriptor => NameEncoder.encodeWithSourceFile(desc)
        case e => throw new Error(s"Not implemented yet: $e")
      }
      if (n.endsWith("Kt")) {
        ApplyStatic(ClassType(n), method, args)(tpe)
      } else {
        val suffix = if (n.endsWith("$")) "" else "$"
        Apply(LoadModule(ClassType(n + suffix)), method, args)(tpe)
      }
    } else {
      val rec = GenExpr(d.getDispatchReceiver, p).tree
      if (isFunction && desc.getName.toString == "invoke") genFunctionCall(desc, rec, args)
      else Apply(rec, method, args)(tpe)
    }
  }

  private def genFunctionCall(desc: CallableDescriptor, rec: Tree, args: List[Tree]) =
    Apply(rec, NameEncoder.encodeApply(desc), args)(desc.getReturnType.toJsType)

  private def genArgs(as: Range): Seq[Tree] = as.map(d.getValueArgument).map {
    case a: IrVararg => GenVararg(a, p).tree
    case a: IrExpression => GenExpr(a, p).tree
    case _ => notImplemented
  }

  private def genClassExtension(desc: SimpleFunctionDescriptor, args: List[Tree]) =
    ApplyStatic(ClassType(NameEncoder.encodeWithSourceFile(desc)), desc.toJsMethodIdent, args)(desc.getReturnType.toJsType)


  private def isTopLevelExtension(sf: SimpleFunctionDescriptor) =
    sf.getExtensionReceiverParameter != null && sf.getContainingDeclaration.getName.asString() == "<root>"

  private def isFunction = {
    val rec = d.getDispatchReceiver
    Try(rec.isInstanceOf[IrCallableReference] ||
      DescriptorUtils.getClassDescriptorForType(rec.getType).isInstanceOf[FunctionClassDescriptor]).fold(_ => false, x => x)
  }
}
