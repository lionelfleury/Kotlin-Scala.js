package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.builtins.KotlinBuiltIns
import org.jetbrains.kotlin.descriptors.VariableDescriptor
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.utils.BindingUtils
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.psi.{KtBinaryExpression, KtForExpression}
import org.jetbrains.kotlin.resolve.DescriptorUtils
import org.jetbrains.kotlin.types.KotlinType
import org.scalajs.core.ir.Trees
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.IntType

case class GenFor(d: KtForExpression)(implicit val c: TranslationContext) extends Gen[KtForExpression] {

  override def tree: Trees.Tree = {
    val ptpe = BindingUtils.getDescriptorForElement(c.bindingContext(), d.getLoopParameter).asInstanceOf[VariableDescriptor]
    val tpe = ptpe.getType.toJsType
    val rangeType = BindingUtils.getTypeForExpression(c.bindingContext(), d.getLoopRange)
    val value: Tree => Tree =
      i => VarDef(Ident(d.getLoopParameter.getName), tpe, mutable = false, i)
    if (KotlinBuiltIns.isArray(rangeType) || KotlinBuiltIns.isPrimitiveArray(rangeType)) {
      val range = GenExpr(d.getLoopRange).tree
      val v: Value = i => value(ArraySelect(range, i)(tpe))
      genFor(BinaryOp.Num_<, IntLiteral(0), ArrayLength(range), IntLiteral(1), v)
    } else if (isForOverRangeLiteral(rangeType)) {
      val range = d.getLoopRange.asInstanceOf[KtBinaryExpression]
      val start = GenExpr(range.getLeft).tree
      val end = GenExpr(range.getRight).tree
      genFor(BinaryOp.Num_<=, start, end, IntLiteral(1), value)
    } else if (isForOverRange(rangeType)) {
      notImplemented
    } else notImplemented
    // TODO: LoopTranslator.kt -> RangeLiteral, Range, Array, Iterator (default)
  }

  private def isForOverRange(rangeType: KotlinType): Boolean = {
    Option(rangeType.getConstructor.getDeclarationDescriptor)
    .map(DescriptorUtils.getFqName)
    .fold(false)(_.asString() == "kotlin.ranges.IntRange")
  }

  private def isForOverRangeLiteral(rangeType: KotlinType): Boolean = d.getLoopRange match {
    case b: KtBinaryExpression => b.getOperationToken == KtTokens.RANGE && isForOverRange(rangeType)
    case _ => false
  }

  type Value = (VarRef) => Tree

  private def genFor(condition: BinaryOp.Code, start: Tree, end: Tree, step: Tree, value: Value): Tree = {
    val i = VarDef(Ident("tmp$"), IntType, mutable = true, start)
    val cond = BinaryOp(condition, i.ref, end)
    val iplus = Assign(i.ref, BinaryOp(BinaryOp.Int_+, i.ref, step))
    val body = Block(List(value(i.ref), GenBody(d.getBody).tree, iplus))
    Block(List(i, While(cond, body, None)))
  }

}
