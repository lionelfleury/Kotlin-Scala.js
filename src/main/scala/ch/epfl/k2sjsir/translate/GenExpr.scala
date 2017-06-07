package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.utils.NameEncoder
import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.impl.{LocalVariableDescriptor, PropertyDescriptorImpl}
import org.jetbrains.kotlin.descriptors._
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.utils.BindingUtils
import org.jetbrains.kotlin.psi._
import org.jetbrains.kotlin.resolve.DescriptorUtils._
import org.jetbrains.kotlin.resolve.`lazy`.descriptors.LazyClassDescriptor
import org.jetbrains.kotlin.resolve.calls.callUtil.CallUtilKt
import org.jetbrains.kotlin.resolve.scopes.receivers.{ExpressionReceiver, ExtensionReceiver, ImplicitClassReceiver}
import org.jetbrains.kotlin.resolve.{BindingContext, DescriptorUtils}
import org.jetbrains.kotlin.types.DynamicTypesKt
import org.scalajs.core.ir.Trees
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.{ArrayType, ClassType, Type}

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
        BindingUtils.getDescriptorForReferenceExpression(c.bindingContext(), kn) match {
          case m: PropertyDescriptor =>
            val tpe = m.getType.toJsType
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
                case x: ExpressionReceiver => Apply(This()(recv.toJsClassType), m.getterIdent(), List())(tpe)
                case _ =>
                  notImplemented
              }
            }
          case lv: LocalVariableDescriptor =>
            val tpe = lv.getType.toJsType
            val ident = Ident(kn.getReferencedNameAsName.toString)
            VarRef(ident)(tpe)
          case vd: ValueParameterDescriptor =>
            val tpe = vd.getType.toJsType
            val ident = Ident(kn.getReferencedNameAsName.toString)
            VarRef(ident)(tpe)
          case lc: LazyClassDescriptor =>
            val external = if(lc.isCompanionObject) {
              lc.getContainingDeclaration match {
                case x: ClassDescriptor => x.isExternal
                case _ => lc.isExternal
              }
            } else lc.isExternal
            if(external) LoadJSModule(lc.toJsClassType)
            else LoadModule(lc.toJsClassType)
          case _ =>
            notImplemented
        }
      case k: KtConstantExpression => GenConst(k).tree
      case k: KtArrayAccessExpression => GenArrayAccess(k).tree
      case k: KtBinaryExpression => GenBinary(k).tree
      case k: KtForExpression => GenFor(k).tree
      case k: KtDotQualifiedExpression =>
        val receiver = GenExpr(k.getReceiverExpression).tree
        val isArray = receiver.tpe.isInstanceOf[ArrayType]
        k.getSelectorExpression match {
          case call: KtCallExpression =>
            val resolved = CallUtilKt.getResolvedCall(call, c.bindingContext())
            val desc = resolved.getResultingDescriptor
            val args =
              resolved.getCall.getValueArguments.asScala.map(x => GenExpr(x.getArgumentExpression).tree).toList
            val tpe = desc.getReturnType.toJsType

            val ao = if(isArray) arrayOps(receiver, tpe, desc.getName.asString(), args) else None
            ao.getOrElse({
              if (GenUnary.isUnaryOp(desc.getName.asString())) {
                val op = GenUnary.convertToOp(receiver.tpe, tpe)
                op.fold(notImplemented)(UnaryOp(_, receiver))
              }
              else if (DescriptorUtils.isExtension(desc)) GenCall(call).genExtensionCall(receiver)
              else {
                receiver match {
                  case _: LoadJSModule | _: JSNew =>
                    JSBracketMethodApply(receiver, StringLiteral(desc.getName.asString()), args)
                  case _ =>
                    val name = if (desc.getName.toString == "invoke") NameEncoder.encodeApply(desc) else desc.toJsMethodIdent
                    Apply(receiver, name, args.toList)(tpe)
                }
              }
            })
          case kn: KtNameReferenceExpression =>
            val tpe = c.bindingContext().getType(kn).toJsType
            val ao = if(isArray) arrayOps(receiver, tpe, kn.getReferencedName, List()) else None
            ao.getOrElse({
              BindingUtils.getDescriptorForReferenceExpression(c.bindingContext(), kn) match {
                case m: PropertyDescriptor => Apply(receiver, m.getterIdent(), List())(tpe)
                case _ => notImplemented
              }
            })
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
      case kp: KtParenthesizedExpression => GenExpr(kp.getExpression).tree
      case kc: KtCallableReferenceExpression =>
        CallUtilKt.getResolvedCall(kc.getCallableReference, c.bindingContext()).getResultingDescriptor match {
          case f: FunctionDescriptor => genClosure(f)
          case _ => notImplemented
        }
      case l: KtLambdaExpression => genLambda(l)
      case w: KtWhileExpression =>
        val body = GenBody(w.getBody).tree
        val cond = GenExpr(w.getCondition).tree
        While(cond, body)
      case _ => notImplemented
    }
  }

  private def arrayOps(receiver: Tree, tpe: Type, method: String, args: List[Tree]) : Option[Tree] = {
    method match {
      case "get" =>
        require(args.size == 1)
        Some(ArraySelect(receiver, args.head)(tpe))
      case "set" =>
        require(args.size == 2)
        Some(Assign(ArraySelect(receiver, args.head)(args(1).tpe), args(1)))
      case "size" =>
        Some(ArrayLength(receiver))
      case _ => None
    }
  }

  private def genLambda(l: KtLambdaExpression) : Tree = {
    val body = GenBody(l.getBodyExpression).treeOption
    val desc = BindingUtils.getFunctionDescriptor(c.bindingContext(), l.getFunctionLiteral)
    genClosure(desc, body)
  }

  private def genClosure(desc: FunctionDescriptor, body: Option[Tree] = None) : Tree = {
    val containingClass = Option(DescriptorUtils.getContainingClass(desc))
    val ct = containingClass.fold(ClassType(NameEncoder.encodeWithSourceFile(desc)))(cc => cc.toJsClassType)

    val b : Tree = body.getOrElse({
      val static = DescriptorUtils.isStaticDeclaration(desc)
      val methodName = desc.toJsMethodIdent
      val parameters = desc.getValueParameters.asScala.map(x => VarRef(Ident(x.getName.toString))(x.getType.toJsClassType)).toList

      if (static) ApplyStatic(ct, methodName, parameters)(desc.getReturnType.toJsType)
      else Apply(VarRef(Ident("$this"))(ct), methodName, parameters)(desc.getReturnType.toJsClassType)
    })


    val closureParams = desc.getValueParameters.asScala.map(_.toJsParamDef).toList
    val closure = containingClass match {
      case Some(cc) =>
        val captureParams = List(ParamDef(Ident("$this"), ct, mutable = false, rest = false))
        val captureValues = List[Trees.Tree](This()(ct))
        val closureParams = desc.getValueParameters.asScala.map(_.toJsParamDef).toList
        Closure(captureParams, closureParams, b, captureValues)
      case None => Closure(List(), closureParams, b, List())
    }

    New(ClassType(s"sjsr_AnonFunction${closureParams.size}"), Ident(s"init___sjs_js_Function${closureParams.size}"), List(closure))
  }

  def treeOption: Option[Tree] = if (d == null) None else Some(tree)

}
