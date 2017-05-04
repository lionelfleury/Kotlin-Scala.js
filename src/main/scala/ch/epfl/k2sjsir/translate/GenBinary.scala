package ch.epfl.k2sjsir.translate

import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.operation.AssignmentTranslator
import org.jetbrains.kotlin.js.translate.utils.PsiUtils
import org.jetbrains.kotlin.psi.KtBinaryExpression
import org.scalajs.core.ir.Trees.{Assign, Tree}

case class GenBinary(d: KtBinaryExpression)(implicit val c: TranslationContext) extends Gen[KtBinaryExpression] {

  override def tree: Tree = {
    val left = GenExpr(d.getLeft).tree
    val right = GenExpr(d.getRight).tree
    val op = PsiUtils.getOperationToken(d)
    if (AssignmentTranslator.isAssignmentOperator(op)) Assign(left, right)
    else
      notImplemented
  }

}
