package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.utils.Utils._
import kotlin.reflect.jvm.internal.impl.resolve.scopes.receivers.ImplicitClassReceiver
import org.jetbrains.kotlin.descriptors.{CallableDescriptor, ClassConstructorDescriptor, SimpleFunctionDescriptor}
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.psi.KtCallExpression
import org.jetbrains.kotlin.resolve.DescriptorUtils
import org.jetbrains.kotlin.resolve.DescriptorUtils._
import org.jetbrains.kotlin.resolve.calls.callUtil.CallUtilKt
import org.jetbrains.kotlin.resolve.scopes.receivers.{ExpressionReceiver, ExtensionReceiver, ReceiverValue}
import org.jetbrains.kotlin.serialization.deserialization.descriptors.DeserializedSimpleFunctionDescriptor
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.{ClassType, NoType}

import scala.collection.JavaConverters._

case class GenCall(d: KtCallExpression)(implicit val c: TranslationContext) extends Gen[KtCallExpression] {

  private val resolved = CallUtilKt.getResolvedCall(d, c.bindingContext())
  private val desc = resolved.getResultingDescriptor
  private lazy val rtpe = desc.getReturnType.toJsType

  override def tree: Tree = {
    val name = desc.getName.asString()
    val x = Option(resolved.getExtensionReceiver).map(x => genExtensionReceiver(x))
    val args = (x ++ genArgs()).toList
    desc match {
      case sf: DeserializedSimpleFunctionDescriptor =>
        if (name == "println" || name == "print") {
          val rec = LoadModule(ClassType("s_Predef$"))
          val method = Ident(s"${name}__O__V", Some(name))
          Apply(rec, method, args)(rtpe)
        }
        else if (GenArray.isArrayOps(desc)) GenArray(d, args).tree
        else notImplemented
      case cc: ClassConstructorDescriptor =>
        val ctpe = if(cc.getContainingDeclaration.getName.toString == "Exception") ClassType("jl_Exception")
        else cc.getContainingDeclaration.toJsClassType
        New(ctpe, desc.toJsMethodIdent, args)
      case sf: SimpleFunctionDescriptor =>
        val dr = desc.getDispatchReceiverParameter
        if(DescriptorUtils.isExtension(desc)) {
          Apply(
            This()(ClassType(getFqName(dr.getContainingDeclaration).asString())),
            desc.toJsMethodIdent,
            VarRef(dr.toJsIdent)(dr.getType.toJsType) :: args
          )(rtpe)
        } else {
          val a = resolved.getDispatchReceiver
          a match {
            case i: ImplicitClassReceiver => notImplemented
            case e: ExtensionReceiver =>
              e.getDeclarationDescriptor
            case e: ExpressionReceiver => GenExpr(e.getExpression).tree
            case _ => notImplemented
          }
          /*Apply(
            ,
            desc.toJsMethodIdent,
            args
          )*/
          notImplemented
        }
      case _ =>
        notImplemented
    }
  }

  private def genExtensionReceiver(r: ReceiverValue) = {
    notImplemented
  }

  private def genArgs(): Seq[Tree] = {
    resolved.getCall.getValueArguments.asScala.map(x => GenExpr(x.getArgumentExpression).tree)
  }

}
