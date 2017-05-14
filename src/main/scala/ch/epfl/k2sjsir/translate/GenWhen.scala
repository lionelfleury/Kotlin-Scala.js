package ch.epfl.k2sjsir.translate

import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.utils.BindingUtils
import org.jetbrains.kotlin.psi._
import org.scalajs.core.ir.Trees._
import ch.epfl.k2sjsir.utils.Utils._
import org.scalajs.core.ir.Trees
import org.scalajs.core.ir.Types._

import scala.collection.JavaConverters._

case class GenWhen(d: KtWhenExpression)(implicit val c: TranslationContext) extends Gen[KtWhenExpression] {

  override def tree: Tree = {
    val sub = GenExpr(d.getSubjectExpression).tree
    val es = d.getEntries.asScala.toList
    genWhenEntry(sub)(es)
  }

  private def genCondition(sub: Tree)(cond: KtWhenCondition): Tree = {
    val eval = cond match {
      case is: KtWhenConditionIsPattern =>
        val tpe = BindingUtils.getTypeByReference(c.bindingContext(), is.getTypeReference)
        IsInstanceOf(sub, getTypeForIs(tpe.toJsInternal))
      case e: KtWhenConditionWithExpression =>
        BinaryOp(BinaryOp.===, sub, GenExpr(e.getExpression).tree)
      case r: KtWhenConditionInRange =>
        notImplemented
      case _ => notImplemented
    }
    if (isNegated(cond)) UnaryOp(UnaryOp.Boolean_!, eval) else eval
  }

  private def genConditions(sub: Tree)(cs: List[KtWhenCondition]): Tree = cs match {
    case h :: Nil => genCondition(sub)(h)
    case h :: t => BinaryOp(BinaryOp.Boolean_|, genCondition(sub)(h), genConditions(sub)(t))
    case _ => BooleanLiteral(true)
  }

  private def genWhenEntry(sub: Tree)(es: List[KtWhenEntry]): Tree = es match {
    case e :: Nil if e.isElse => GenExpr(e.getExpression).tree
    case h :: t =>
      val cond = genConditions(sub)(h.getConditions.toList)
      val exp = GenExpr(h.getExpression).tree
      If(cond, exp, genWhenEntry(sub)(t))(exp.tpe)
    case _ => Undefined()
  }

  private def isNegated(condition: KtWhenCondition) = condition match {
    case c: KtWhenConditionIsPattern => c.isNegated
    case _ => false
  }

  private def getTypeForIs(tpe: String): ReferenceType = tpe match {
    case "I" => ClassType("jl_Integer")
    case "D" => ClassType("jl_Double")
    case "F" => ClassType("jl_Float")
    case "Z" => ClassType("jl_Boolean")
    case "J" => ClassType("jl_Long")
    case other => ClassType(other)
  }

}
