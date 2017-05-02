package ch.epfl.k2sjsir.translate

import org.jetbrains.kotlin.builtins.KotlinBuiltIns
import org.jetbrains.kotlin.descriptors.VariableDescriptor
import org.jetbrains.kotlin.js.translate.context.{Namer, TranslationContext}
import org.jetbrains.kotlin.js.translate.utils.BindingUtils
import org.jetbrains.kotlin.psi.{KtExpression, KtForExpression}
import org.scalajs.core.ir.{Definitions, Trees, Types}
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.IntType
import ch.epfl.k2sjsir.utils.Utils._

case class GenFor(d: KtForExpression)(implicit val c: TranslationContext) extends Gen[KtForExpression] {

  override def tree: Trees.Tree = {
    val ptpe = BindingUtils.getDescriptorForElement(c.bindingContext(), d.getLoopParameter).asInstanceOf[VariableDescriptor]
    val tpe = ptpe.getType.toJsType
    val range = GenExpr(d.getLoopRange).tree
    val rangeType = BindingUtils.getTypeForExpression(c.bindingContext(), d.getLoopRange)
    if (KotlinBuiltIns.isArray(rangeType) || KotlinBuiltIns.isPrimitiveArray(rangeType)) {
      val index = VarDef(Ident("tmp$"), IntType, true, Types.zeroOf(IntType))
      val cond = BinaryOp(BinaryOp.Num_<, index.ref, ArrayLength(range))
      val iplus = Assign(index.ref, BinaryOp(BinaryOp.Int_+, index.ref, IntLiteral(1)))
      val value = VarDef(Ident(d.getLoopParameter.getName), tpe, false, ArraySelect(range, index.ref)(tpe))
      val body = GenBody(d.getBody).tree match {
        case b: Block => Block(value +: b.stats :+ iplus)
        case b: Tree => Block(List(value, b, iplus))
      }
      Block(List(index, While(cond, body, None)))
    } else notImplemented
    // TODO: LoopTranslator.kt -> RangeLiteral, Range, Array, Iterator (default)
  }

}
