package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.impl.LocalVariableDescriptor
import org.jetbrains.kotlin.descriptors.{PropertyDescriptor, ValueParameterDescriptor, VariableDescriptor}
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.utils.BindingUtils
import org.jetbrains.kotlin.psi._
import org.jetbrains.kotlin.resolve.BindingContext
import org.jetbrains.kotlin.resolve.DescriptorUtils._
import org.jetbrains.kotlin.resolve.calls.callUtil.CallUtilKt
import org.scalajs.core.ir.Trees._

import scala.collection.JavaConverters._

case class GenExpr(d: KtExpression)(implicit val c: TranslationContext) extends Gen[KtExpression] {

  override def tree: Tree = {
    d match {
      case ce: KtCallExpression => GenCall(ce).tree
      case ks: KtStringTemplateExpression =>
        ks.getEntries.foldLeft(StringLiteral(""): Tree)((acc, expr) => {
          BinaryOp(BinaryOp.String_+, acc, expr match {
            case sl: KtLiteralStringTemplateEntry => StringLiteral(sl.getText)
            case st: KtStringTemplateEntry => GenExpr(st.getExpression).tree
            case _ => notImplemented
          })
        })
      case kp: KtProperty =>
        val expr = GenExpr(kp.getDelegateExpressionOrInitializer).tree
        val desc = c.bindingContext().get(BindingContext.VARIABLE, kp)
        VarDef(desc.toJsIdent, expr.tpe, kp.isVar, expr)
      case kn: KtNameReferenceExpression =>
        val tpe = c.bindingContext().getType(kn).toJsType
        BindingUtils.getDescriptorForReferenceExpression(c.bindingContext(), kn) match {
          case m: PropertyDescriptor =>
            val recvType = getClassDescriptorForType(m.getDispatchReceiverParameter.getValue.getType).toJsClassType
            Apply(This()(recvType), m.getterIdent(), List())(tpe)
          case _: LocalVariableDescriptor | _: ValueParameterDescriptor =>
            val ident = Ident(kn.getReferencedNameAsName.toString)
            VarRef(ident)(tpe)
          case _ =>
            notImplemented
        }
      case k: KtConstantExpression => GenConst(k).tree
      case k: KtArrayAccessExpression => GenArrayAccess(k).tree
      case k: KtBinaryExpression => GenBinary(k).tree
      case k: KtForExpression => GenFor(k).tree
      case k: KtDotQualifiedExpression =>
        val receiver = GenExpr(k.getReceiverExpression).tree
        k.getSelectorExpression match {
          case call: KtCallExpression =>
            val resolved = CallUtilKt.getResolvedCall(call, c.bindingContext())
            val desc = resolved.getResultingDescriptor
            val ext = desc.getDispatchReceiverParameter //TODO: Handle dispatch receiver
            val extType = ext.getValue.getType
            val tpe = desc.getReturnType.toJsType
            if (GenUnary.isUnaryOp(desc.getName.asString())) {
              val op = GenUnary.convertToOp(receiver.tpe, tpe)
              op.fold(notImplemented)(UnaryOp(_, receiver))
            } else {
              val args =
                resolved.getCall.getValueArguments.asScala.map(x => GenExpr(x.getArgumentExpression).tree)
              Apply(receiver, desc.toJsMethodIdent, args.toList)(tpe)
            }
          case _ => notImplemented
        }
      case k: KtUnaryExpression => GenUnary(k).tree
      case k: KtTryExpression =>
        val content = GenBody(k.getTryBlock).tree
        val catches = k.getCatchClauses.asScala.map(ctch => ({
          val d = ctch.getCatchParameter
          val ptpe = BindingUtils.getDescriptorForElement(c.bindingContext(), d).asInstanceOf[VariableDescriptor]
          val tpe = ptpe.getType.toJsType
          VarRef(Ident(d.getName))(tpe)
        }, GenBody(ctch.getCatchBody).tree))
        val fnl = Option(k.getFinallyBlock).map(x => GenBody(x.getFinalExpression).tree)

        val tc = catches.foldLeft(content) { case (b, (p, h)) => TryCatch(b, p.ident, h)(b.tpe) }
        if (fnl.nonEmpty) TryFinally(tc, fnl.get) else tc
      case k: KtThrowExpression => Throw(GenExpr(k.getThrownExpression).tree)
      case _ => notImplemented
    }
  }

  def treeOption: Option[Tree] = if (d == null) None else Some(tree)

}
