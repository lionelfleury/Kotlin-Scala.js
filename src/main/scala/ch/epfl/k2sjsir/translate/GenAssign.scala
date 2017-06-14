package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.impl.LocalVariableDescriptor
import org.jetbrains.kotlin.descriptors.{ClassDescriptor, DeclarationDescriptor, PropertyDescriptor}
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.reference.{AccessTranslationUtils, ArrayAccessTranslator, BackingFieldAccessTranslator, VariableAccessTranslator}
import org.jetbrains.kotlin.js.translate.utils.BindingUtils._
import org.jetbrains.kotlin.js.translate.utils.PsiUtils._
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.psi._
import org.jetbrains.kotlin.resolve.calls.callUtil.CallUtilKt
import org.jetbrains.kotlin.resolve.scopes.receivers.{ClassValueReceiver, ExpressionReceiver}
import org.jetbrains.kotlin.types.DynamicTypesKt
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.NoType


case class GenAssign(d: KtBinaryExpression)(implicit val c: TranslationContext) extends Gen[KtBinaryExpression] {

  private val desc = getCallableDescriptorForOperationExpression(c.bindingContext(), d)
  private val left = d.getLeft
  private val tpe = if (desc != null) desc.getReturnType.toJsType else GenExpr(left).tree.tpe
  private val right = GenExpr(d.getRight).tree

  override def tree: Tree = {
    if (isReferenceToBackingFieldFromConstructor(left, c)) {
      val simpleName: KtSimpleNameExpression = getSimpleName(left)
      val a = BackingFieldAccessTranslator.newInstance(simpleName, c)
      notImplemented
    }
    else {
      AccessTranslationUtils.getAccessTranslator(left, c) match {
        case v: VariableAccessTranslator =>
          val call = CallUtilKt.getResolvedCallWithAssert(left, c.bindingContext())
          call.getResultingDescriptor match {
            case p: PropertyDescriptor =>
              val receiverExpr : KtExpression = call.getDispatchReceiver match {
                case cl: ClassValueReceiver => cl.getExpression
                case e: ExpressionReceiver => e.getExpression
                case _ =>
                  notImplemented
                  throw new Exception("")
              }
              val receiver = GenExpr(receiverExpr).tree
              val rcvType = c.bindingContext().getType(receiverExpr)
              d.getOperationToken match {
                case KtTokens.PLUSEQ | KtTokens.MINUSEQ | KtTokens.MULTEQ | KtTokens.DIVEQ | KtTokens.PERCEQ =>
                  val code = d.getOperationToken.toString.replaceAll("[T]?EQ", "")
                  val binOp = GenBinary.getBinaryOp(code, tpe)
                  val args = BinaryOp(binOp, Apply(receiver, p.getGetter.toJsMethodIdent, List())(tpe), right)
                  Apply(receiver, p.getSetter.toJsMethodIdent, List(args))(NoType)
                case KtTokens.EQ if DynamicTypesKt.isDynamic(rcvType) || receiver.isJSReceiver =>
                  Assign(JSBracketSelect(receiver, StringLiteral(p.getName.asString())), right)
                case KtTokens.EQ =>
                  Apply(receiver, p.getSetter.toJsMethodIdent, List(right))(NoType)
              }
            case l: LocalVariableDescriptor =>
              val ref = VarRef(l.toJsIdent)(l.getType.toJsType)
              d.getOperationToken match {
                case KtTokens.PLUSEQ | KtTokens.MINUSEQ | KtTokens.MULTEQ | KtTokens.DIVEQ | KtTokens.PERCEQ =>
                  val code = d.getOperationToken.toString.replaceAll("[T]?EQ", "")
                  val binOp = GenBinary.getBinaryOp(code, tpe)
                  Assign(VarRef(l.toJsIdent)(l.getType.toJsType), BinaryOp(binOp, ref, right))
                case KtTokens.EQ =>
                  Assign(VarRef(l.toJsIdent)(l.getType.toJsType), right)
              }
          }
        case a: ArrayAccessTranslator =>
          if(d.getOperationToken == KtTokens.EQ) Assign(GenExpr(left).tree, right)
          else ArraySelect(GenExpr(left).tree, right)(tpe)
      }
    }
  }

  private def isReferenceToBackingFieldFromConstructor(expression: KtExpression, context: TranslationContext): Boolean = {
    expression match {
      case _: KtSimpleNameExpression =>
        val nameExpression: KtSimpleNameExpression = expression.asInstanceOf[KtSimpleNameExpression]
        val descriptor: DeclarationDescriptor = getDescriptorForReferenceExpression(context.bindingContext, nameExpression)
        isReferenceToBackingFieldFromConstructor(descriptor, context)
      case _: KtDotQualifiedExpression =>
        val qualifiedExpression: KtDotQualifiedExpression = expression.asInstanceOf[KtDotQualifiedExpression]
        if (qualifiedExpression.getReceiverExpression.isInstanceOf[KtThisExpression] && qualifiedExpression.getSelectorExpression.isInstanceOf[KtSimpleNameExpression]) {
          val nameExpression: KtSimpleNameExpression = qualifiedExpression.getSelectorExpression.asInstanceOf[KtSimpleNameExpression]
          val descriptor: DeclarationDescriptor = getDescriptorForReferenceExpression(context.bindingContext, nameExpression)
          isReferenceToBackingFieldFromConstructor(descriptor, context)
        } else false
      case _ => false
    }
  }

  private def isReferenceToBackingFieldFromConstructor(descriptor: DeclarationDescriptor, context: TranslationContext): Boolean = {
    if (!descriptor.isInstanceOf[PropertyDescriptor]) return false
    val propertyDescriptor: PropertyDescriptor = descriptor.asInstanceOf[PropertyDescriptor]
    if (!context.getDeclarationDescriptor.isInstanceOf[ClassDescriptor]) return false
    val classDescriptor: ClassDescriptor = context.getDeclarationDescriptor.asInstanceOf[ClassDescriptor]
    if (classDescriptor ne propertyDescriptor.getContainingDeclaration) return false
    return !propertyDescriptor.isVar
  }
}
