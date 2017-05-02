package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.codegen.GenUnaryOp.{apply => _}
import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.psi.KtCallExpression
import org.jetbrains.kotlin.resolve.calls.callUtil.CallUtilKt
import org.jetbrains.kotlin.resolve.scopes.receivers.ReceiverValue
import org.jetbrains.kotlin.serialization.deserialization.descriptors.DeserializedSimpleFunctionDescriptor
import org.jetbrains.kotlin.types.TypeUtils
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.{ArrayType, ClassType}

import scala.collection.JavaConverters._

case class GenCall(d: KtCallExpression)(implicit val c: TranslationContext) extends Gen[KtCallExpression] {

  private val resolved = CallUtilKt.getResolvedCall(d, c.bindingContext())
  private val desc = resolved.getCandidateDescriptor

  override def tree: Tree = {
    val name = desc.getName.asString()
    val x = Option(resolved.getExtensionReceiver).map(x => genExtensionReceiver(x))
    val args = (x ++ genArgs()).toList
    desc match {
      case sf: DeserializedSimpleFunctionDescriptor =>
        if (name == "println" || name == "print") {
          val rec = LoadModule(ClassType("s_Predef$"))
          val method = Ident(s"${name}__O__V", Some(name))
          val tpe = sf.getReturnType.toJsType
          Apply(rec, method, args)(tpe)
        }
        else if (GenArray.isArrayOps(desc)) GenArray(d, args).tree
        else notImplemented
      /*else if (isBinaryOp(name)) GenBinaryOp(d, p).tree
      else if (isUnaryOp(name)) GenUnaryOp(d, p).tree
      else genApply(sf, args.toList)*/
    }
  }

  private def genExtensionReceiver(r: ReceiverValue) = {
    notImplemented
  }

  private def genArgs(): Seq[Tree] = {
    resolved.getCall.getValueArguments.asScala.map(x => GenExpr(x.getArgumentExpression).tree)
  }

}
