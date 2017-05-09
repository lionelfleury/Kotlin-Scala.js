package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.{CallableDescriptor, ClassConstructorDescriptor, SimpleFunctionDescriptor}
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.psi.KtCallExpression
import org.jetbrains.kotlin.resolve.DescriptorUtils
import org.jetbrains.kotlin.resolve.DescriptorUtils._
import org.jetbrains.kotlin.resolve.calls.callUtil.CallUtilKt
import org.jetbrains.kotlin.resolve.scopes.receivers.ReceiverValue
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
        if(DescriptorUtils.isExtension(desc)) {
          val dr = desc.getDispatchReceiverParameter
          Apply(
            This()(ClassType(getFqName(dr.getContainingDeclaration).asString())),
            desc.toJsMethodIdent,
            List(
              VarRef(dr.toJsIdent)(dr.getType.toJsType)
            ))(rtpe)
        } else
          notImplemented
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
