package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.psi._
import org.jetbrains.kotlin.resolve.BindingContext
import org.scalajs.core.ir.Trees._

case class GenExpr(d: KtExpression)(implicit val c: TranslationContext) extends Gen[KtExpression] {

  override def tree: Tree = d match {
    case ce: KtCallExpression => GenCall(ce).tree
    case ks: KtStringTemplateExpression =>
      val string = ks.getEntries.foldLeft("")((acc, expr) => {
        acc + (expr match {
          case sl: KtLiteralStringTemplateEntry => sl.getText
          case st: KtStringTemplateEntry => ""
          case _ => ""
        })
      })
      StringLiteral(string)
    case kp: KtProperty =>
      kp.getDelegateExpressionOrInitializer match {
        case k: KtConstantExpression =>
          val expr = GenConst(k).tree
          val desc = c.bindingContext().get(BindingContext.VARIABLE, kp)
          VarDef(desc.toJsIdent, c.bindingContext().getType(k).toJsType, mutable = false, expr)
        case _ => notImplemented
      }
    case _ => notImplemented
  }


  def treeOption: Option[Tree] = if (d == null) None else Some(tree)
}