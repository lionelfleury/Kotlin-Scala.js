package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.utils.Utils
import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.utils.BindingUtils
import org.jetbrains.kotlin.psi.KtIsExpression
import org.scalajs.core.ir.Trees
import org.scalajs.core.ir.Trees.{IsInstanceOf, UnaryOp}

case class GenIs(d: KtIsExpression)(implicit val c: TranslationContext) extends Gen[KtIsExpression] {
  override def tree: Trees.Tree = {
    val lhs = GenExpr(d.getLeftHandSide).tree
    val typeRef = BindingUtils.getTypeByReference(c.bindingContext(), d.getTypeReference).toJsInternal
    val isTypeRef = Utils.getTypeForIs(typeRef)
    val isNegated = d.isNegated

    val scalaEquivOp = IsInstanceOf(lhs, isTypeRef)

    if (isNegated)
      UnaryOp(UnaryOp.Boolean_!, scalaEquivOp)
    else
      scalaEquivOp
  }
}
