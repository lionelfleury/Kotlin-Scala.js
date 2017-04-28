package ch.epfl.k2sjsir.translate

import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.psi._
import org.jetbrains.kotlin.resolve.constants.IntegerValueTypeConstant
import org.jetbrains.kotlin.resolve.constants.evaluate.ConstantExpressionEvaluator
import org.scalajs.core.ir.Trees._

case class GenConst(d: KtConstantExpression)(implicit val c: TranslationContext) extends Gen[KtConstantExpression] {

  override def tree: Tree = {
    val exprType = c.bindingContext().getType(d)
    ConstantExpressionEvaluator.getConstant(d, c.bindingContext()) match {
      case i: IntegerValueTypeConstant => IntLiteral(i.getValue(exprType).asInstanceOf[Int])
      case _ => notImplemented
    }
  }


  def treeOption: Option[Tree] = if (d == null) None else Some(tree)
}