package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.utils.NameEncoder
import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.impl.{LocalVariableDescriptor, SyntheticFieldDescriptor}
import org.jetbrains.kotlin.descriptors.{ClassDescriptor, DeclarationDescriptor, PropertyDescriptor}
import org.jetbrains.kotlin.fileClasses.JvmFileClassUtil
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.reference.{AccessTranslationUtils, ArrayAccessTranslator, BackingFieldAccessTranslator, VariableAccessTranslator}
import org.jetbrains.kotlin.js.translate.utils.BindingUtils._
import org.jetbrains.kotlin.js.translate.utils.PsiUtils._
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.psi._
import org.jetbrains.kotlin.resolve.DescriptorUtils
import org.jetbrains.kotlin.resolve.calls.callUtil.CallUtilKt
import org.jetbrains.kotlin.resolve.scopes.receivers._
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.{ClassType, IntType, NoType}


case class GenAssign(d: KtBinaryExpression)(implicit val c: TranslationContext) extends Gen[KtBinaryExpression] {

  private val desc = getCallableDescriptorForOperationExpression(c.bindingContext(), d)
  private val left = d.getLeft
  private val tpe = if (desc != null) desc.getReturnType.toJsType else GenExpr(left).tree.tpe
  private val right = GenExpr(d.getRight).tree

  override def tree: Tree = {
    if (isReferenceToBackingFieldFromConstructor(left, c)) {
      val simpleName: KtSimpleNameExpression = getSimpleName(left)
      val a = BackingFieldAccessTranslator.newInstance(simpleName, c)
      notImplemented("when generating backing field from constructor")
    }
    else {
      val resolvedCall = CallUtilKt.getResolvedCallWithAssert(left, c.bindingContext())

      resolvedCall.getResultingDescriptor match {
        case s: SyntheticFieldDescriptor =>
          val backingField = {
            val property = s.getPropertyDescriptor
            val thisTpe = DescriptorUtils.getClassDescriptorForType(property.getDispatchReceiverParameter.getType).toJsClassType
            val name = property.toJsName
            val rcv = genThisFromContext(thisTpe)

            Select(rcv, Ident(name))(tpe)
          }

          Assign(backingField, right)

        case p: PropertyDescriptor =>
          val isSuperCall = resolvedCall.getDispatchReceiver.isInstanceOf[SuperCallReceiverValue]

          val receiver = resolvedCall.getDispatchReceiver match {
            case s: SuperCallReceiverValue => genThisFromContext(s.getType.toJsType, p)
            case t: ThisClassReceiver => genThisFromContext(t.getType.toJsType, p)
            case cl: ClassValueReceiver => GenExpr(cl.getExpression).tree
            case e: ExpressionReceiver => GenExpr(e.getExpression).tree
            case _ if p.isTopLevel => null // Receiver is not used later in this case
            case _ => notImplemented("Unhandled receiver type (from PropertyDescriptor)")
          }

          val clsSuperTpe = receiver.tpe match {
            case c: ClassType => c
            case t if isSuperCall => throw new Exception(s"Super call found with type $t")
            case _ => null
          }

          val isExternal = p.hasExternalParent

          val methodIdent = p.getSetter.toJsMethodIdent
          val callRtpe = NoType

          val ret = d.getOperationToken match {
            case KtTokens.PLUSEQ | KtTokens.MINUSEQ | KtTokens.MULTEQ | KtTokens.DIVEQ | KtTokens.PERCEQ =>
              val code = d.getOperationToken.toString.replaceAll("[T]?EQ", "")
              val binOp = GenBinary.getBinaryOp(code, tpe)
              val args = BinaryOp(binOp, Apply(receiver, p.getGetter.toJsMethodIdent, List())(tpe), right)

              if (p.isTopLevel) {
                val clsTpe = d.getContainingKtFile.toJsClassType
                ApplyStatic(clsTpe, methodIdent, List(args))(callRtpe)

              }
              else if (isSuperCall) {
                ApplyStatically(receiver, clsSuperTpe, methodIdent, List(args))(callRtpe)
              } else {
                Apply(receiver, methodIdent, List(args))(callRtpe)
              }

            case KtTokens.EQ =>
              if (p.isTopLevel) {
                val clsTpe = d.getContainingKtFile.toJsClassType
                ApplyStatic(clsTpe, methodIdent, List(right))(callRtpe)

              }
              else if (isSuperCall) {
                ApplyStatically(receiver, clsSuperTpe, methodIdent, List(right))(callRtpe)
              } else {
                if (isExternal) {
                  val slct = JSBracketSelect(receiver, StringLiteral(p.getName.asString()))
                  Assign(slct, right)
                }
                else
                  Apply(receiver, methodIdent, List(right))(callRtpe)
              }
          }

          ret
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

        case _ =>
          AccessTranslationUtils.getAccessTranslator(left, c) match {
            case a: ArrayAccessTranslator =>
              if(d.getOperationToken == KtTokens.EQ) Assign(GenExpr(left).tree, right)
              else ArraySelect(GenExpr(left).tree, right)(tpe)

            case t => notImplemented(s"After acces translator (${t.toString})")
          }
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
