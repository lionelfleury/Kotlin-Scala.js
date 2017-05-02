package ch.epfl.k2sjsir.translate

import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.psi.{KtBlockExpression, KtCallExpression, KtExpression}
import org.scalajs.core.ir.Trees._

import scala.collection.JavaConverters._

case class GenBody(d: KtExpression)(implicit val c: TranslationContext) extends Gen[KtExpression] {

  override def tree: Tree = d match {
    case be: KtBlockExpression =>
      Block(be.getStatements.asScala.map(x => GenExpr(x).tree).toList)
    case e: KtExpression => Block(GenExpr(e).tree)
    case _ => notImplemented
  }

  def treeOption: Option[Tree] = if (d == null) None else Some(tree)

}
