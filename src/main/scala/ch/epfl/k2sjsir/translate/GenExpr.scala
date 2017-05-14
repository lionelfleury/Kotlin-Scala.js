package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.utils.NameEncoder._
import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.impl.LocalVariableDescriptor
import org.jetbrains.kotlin.descriptors.{PropertyDescriptor, ValueParameterDescriptor, VariableDescriptor}
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.reference.AccessTranslationUtils
import org.jetbrains.kotlin.js.translate.utils.BindingUtils
import org.jetbrains.kotlin.psi._
import org.jetbrains.kotlin.resolve.`lazy`.descriptors.LazyClassDescriptor
import org.jetbrains.kotlin.resolve.{BindingContext, DescriptorUtils}
import org.jetbrains.kotlin.resolve.DescriptorUtils._
import org.jetbrains.kotlin.resolve.calls.callUtil.CallUtilKt
import org.jetbrains.kotlin.resolve.scopes.receivers.{ExtensionReceiver, ImplicitClassReceiver}
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.{ClassType, NoType}

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
            val recv = getClassDescriptorForType(m.getDispatchReceiverParameter.getValue.getType)
            val isObj = recv.isCompanionObject || DescriptorUtils.isObject(recv)
            if(isObj) Apply(LoadModule(recv.toJsClassType), m.getterIdent(), List())(tpe)
            else if(DescriptorUtils.isLocal(m)) VarRef(m.toJsIdent)(recv.toJsClassType)
            else {
              val a = CallUtilKt.getResolvedCallWithAssert(d, c.bindingContext())
              a.getDispatchReceiver match {
                case x: ExtensionReceiver =>
                  val receiver =
                    Option(x.getDeclarationDescriptor.getDispatchReceiverParameter)
                      .getOrElse(x.getDeclarationDescriptor.getExtensionReceiverParameter)
                  val ref = VarRef(receiver.toJsIdent)(x.getType.toJsType)
                  Apply(ref, m.getterIdent(), List())(tpe)
                case _: ImplicitClassReceiver =>
                  Apply(This()(recv.toJsClassType), m.getterIdent(), List())(tpe)
                case _ => notImplemented
              }
            }
          case _: LocalVariableDescriptor | _: ValueParameterDescriptor =>
            val ident = Ident(kn.getReferencedNameAsName.toString)
            VarRef(ident)(tpe)
          case lc: LazyClassDescriptor => LoadModule(lc.toJsClassType)
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
            val args =
              resolved.getCall.getValueArguments.asScala.map(x => GenExpr(x.getArgumentExpression).tree)
            val tpe = desc.getReturnType.toJsType
            if (GenUnary.isUnaryOp(desc.getName.asString())) {
              val op = GenUnary.convertToOp(receiver.tpe, tpe)
              op.fold(notImplemented)(UnaryOp(_, receiver))
            }
            else if(DescriptorUtils.isExtension(desc)) GenCall(call).genExtensionCall(receiver)
            else Apply(receiver, desc.toJsMethodIdent, args.toList)(tpe)
          case kn: KtNameReferenceExpression =>
            val tpe = c.bindingContext().getType(kn).toJsType
            BindingUtils.getDescriptorForReferenceExpression(c.bindingContext(), kn) match {
              case m: PropertyDescriptor => Apply(receiver, m.getterIdent(), List())(tpe)
              case _ => notImplemented
            }
          case _ =>
            notImplemented
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
      case k: KtIfExpression =>
        val cond = GenExpr(k.getCondition).tree
        val thenB = GenExpr(k.getThen).tree
        val elseB = Option(k.getElse).map(x => GenExpr(x).tree)
        If(cond, thenB, elseB.getOrElse(Skip()))(thenB.tpe)
      case k: KtReturnExpression => Return(GenExpr(k.getReturnedExpression).tree)
      case k: KtWhenExpression => GenWhen(k).tree
      case k: KtSafeQualifiedExpression => notImplemented
      case _ => notImplemented
    }
  }

  def treeOption: Option[Tree] = if (d == null) None else Some(tree)

}
