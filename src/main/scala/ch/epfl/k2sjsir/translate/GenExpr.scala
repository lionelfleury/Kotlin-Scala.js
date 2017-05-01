package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.psi._
import org.jetbrains.kotlin.resolve.BindingContext
import org.scalajs.core.ir.Trees._

case class GenExpr(d: KtExpression)(implicit val c: TranslationContext) extends Gen[KtExpression] {

  override def tree: Tree =  {
    d match {
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
        val expr = GenExpr(kp.getDelegateExpressionOrInitializer).tree
        val desc = c.bindingContext().get(BindingContext.VARIABLE, kp)
        VarDef(desc.toJsIdent, expr.tpe, mutable = false, expr)
      case kn: KtNameReferenceExpression =>
        val tpe = c.bindingContext().getType(kn).toJsType
        VarRef(Ident(kn.getReferencedNameAsName.toString))(tpe)
      case k: KtConstantExpression => GenConst(k).tree
      case _ => notImplemented
    }
  }


  def treeOption: Option[Tree] = if (d == null) None else Some(tree)
}