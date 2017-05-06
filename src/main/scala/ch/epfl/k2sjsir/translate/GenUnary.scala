package ch.epfl.k2sjsir.translate

import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.psi.KtUnaryExpression
import org.scalajs.core.ir.Trees

case class GenUnary(d: KtUnaryExpression)(implicit val c: TranslationContext) extends Gen[KtUnaryExpression] {

  override def tree: Trees.Tree = {
    notImplemented
  }

}
