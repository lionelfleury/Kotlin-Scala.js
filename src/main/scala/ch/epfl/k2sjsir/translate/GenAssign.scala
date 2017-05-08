package ch.epfl.k2sjsir.translate

import org.jetbrains.kotlin.descriptors.impl.LocalVariableDescriptor
import org.jetbrains.kotlin.descriptors.{ClassDescriptor, DeclarationDescriptor, PropertyDescriptor}
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.reference.{AccessTranslationUtils, BackingFieldAccessTranslator, VariableAccessTranslator}
import org.jetbrains.kotlin.js.translate.utils.BindingUtils._
import org.jetbrains.kotlin.js.translate.utils.PsiUtils._
import org.jetbrains.kotlin.psi._
import org.jetbrains.kotlin.resolve.calls.callUtil.CallUtilKt
import org.scalajs.core.ir.Trees._
import ch.epfl.k2sjsir.utils.Utils._
import org.scalajs.core.ir.Types.{ClassType, NoType}


case class GenAssign(d: KtBinaryExpression)(implicit val c: TranslationContext) extends Gen[KtBinaryExpression] {

  private val desc = getCallableDescriptorForOperationExpression(c.bindingContext(), d)
  private val left = d.getLeft
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
          call.getCandidateDescriptor match {
            case p: PropertyDescriptor =>
              //ApplyStatic(ClassType("LTest$"),  p.getSetter.toJsMethodIdent, List(right))(NoType)
              Apply(, p.getSetter.toJsMethodIdent, List(right))
            case l: LocalVariableDescriptor => Assign(VarRef(l.toJsIdent)(l.getType.toJsType), right)
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
    if (!(descriptor.isInstanceOf[PropertyDescriptor])) return false
    val propertyDescriptor: PropertyDescriptor = descriptor.asInstanceOf[PropertyDescriptor]
    if (!(context.getDeclarationDescriptor.isInstanceOf[ClassDescriptor])) return false
    val classDescriptor: ClassDescriptor = context.getDeclarationDescriptor.asInstanceOf[ClassDescriptor]
    if (classDescriptor ne propertyDescriptor.getContainingDeclaration) return false
    return !propertyDescriptor.isVar
  }
}
