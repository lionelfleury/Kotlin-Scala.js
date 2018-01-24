package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.utils.Utils
import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.builtins.KotlinBuiltIns
import org.jetbrains.kotlin.descriptors.VariableDescriptor
import org.jetbrains.kotlin.js.backend.ast.JsBinaryOperator
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.utils.{BindingUtils, PsiUtils}
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.psi.{KtBinaryExpression, KtExpression, KtForExpression, KtPsiUtil}
import org.jetbrains.kotlin.resolve.DescriptorUtils
import org.jetbrains.kotlin.resolve.calls.callUtil.CallUtilKt
import org.jetbrains.kotlin.resolve.scopes.receivers.ExpressionReceiver
import org.jetbrains.kotlin.types.KotlinType
import org.scalajs.core.ir.Trees
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.IntType

case class GenFor(d: KtForExpression)(implicit val c: TranslationContext) extends Gen[KtForExpression] {

  type Value = (VarRef) => Tree

  class RangeType
  case class RangeTo() extends RangeType
  case class Until() extends RangeType
  case class DownTo() extends RangeType

  case class RangeLiteral(tpe: RangeType, first: KtExpression, second: KtExpression, step: KtExpression)


  private val rangeToFunctionName = new FqName("kotlin.Int.rangeTo")
  private val untilFunctionName = new FqName("kotlin.ranges.until")
  private val downToFunctionName = new FqName("kotlin.ranges.downTo")
  private val stepFunctionName = new FqName("kotlin.ranges.step")
  private val intRangeName = new FqName("kotlin.ranges.IntRange")
  private val intProgressionName = new FqName("kotlin.ranges.IntProgression")

  override def tree: Trees.Tree = {
    val loopParamDesc = BindingUtils.getDescriptorForElement(c.bindingContext(), d.getLoopParameter).asInstanceOf[VariableDescriptor]
    val tpe = loopParamDesc.getType.toJsType
    val rangeType = BindingUtils.getTypeForExpression(c.bindingContext(), d.getLoopRange)

    val value: Tree => Tree =
      i => VarDef(loopParamDesc.toJsIdent, tpe, mutable = false, i)

    if (KotlinBuiltIns.isArray(rangeType) || KotlinBuiltIns.isPrimitiveArray(rangeType)) {
      val range = GenExpr(d.getLoopRange).tree
      val v: Value = i => value(ArraySelect(range, i)(tpe))
      // Replace this with BinaryOp.Int_< ?
      genFor(BinaryOp.Int_<, IntLiteral(0), ArrayLength(range), IntLiteral(1), v, BinaryOp.Int_+)

    } else if (isForOverRangeLiteral(rangeType)) {
      val rangeLiteral = extractRangeLiteral(rangeType)

      //val range = d.getLoopRange.asInstanceOf[KtBinaryExpression]
      val start = GenExpr(rangeLiteral.first).tree
      val end = GenExpr(rangeLiteral.second).tree
      val step = if (rangeLiteral.step == null) IntLiteral(1) else GenExpr(rangeLiteral.step).tree

      val (binaryOp, incrementOp) = rangeLiteral.tpe match {
        case _: RangeTo => (BinaryOp.Int_<=, BinaryOp.Int_+)
        case _: Until => (BinaryOp.Int_<, BinaryOp.Int_+)
        case _: DownTo => (BinaryOp.Int_>=, BinaryOp.Int_-)
      }

      // Replace this with BinaryOp.Int_<= ?
      genFor(binaryOp, start, end, step, value, incrementOp)

    } else if (isForOverRange(rangeType)) {
      notImplemented("while generating isForOverRange")

    } else notImplemented("for unhandled for type")
    // TODO: LoopTranslator.kt -> RangeLiteral, Range, Array, Iterator (default)
  }

  private def genFor(condition: BinaryOp.Code, start: Tree, end: Tree, step: Tree, value: Value, incrementOp: BinaryOp.Code): Tree = {
    val freshName = Utils.getFreshName("tmp$")
    val i = VarDef(Ident(freshName), IntType, mutable = true, start)
    val cond = BinaryOp(condition, i.ref, end)
    val increment = Assign(i.ref, BinaryOp(incrementOp, i.ref, step))
    val body = Block(List(value(i.ref), GenBody(d.getBody).tree, increment))

    Block(List(i, While(cond, body, None)))
  }

  private def isForOverRange(rangeType: KotlinType): Boolean = {
    Option(rangeType.getConstructor.getDeclarationDescriptor)
    .map(DescriptorUtils.getFqName)
    .fold(false)(_ == intRangeName)
  }

  private def isForOverRangeLiteral(rangeType: KotlinType): Boolean = d.getLoopRange match {
    case b: KtBinaryExpression => extractRangeLiteral(rangeType) != null
    case _ => false
  }

  // Taken from LoopTranslator
  private def extractRangeLiteral(rangeType: KotlinType): RangeLiteral = {
    val fqName = Option(rangeType.getConstructor.getDeclarationDescriptor)
      .map(DescriptorUtils.getFqNameSafe)

    if (fqName.nonEmpty && fqName.get != intRangeName && fqName.get != intProgressionName)
      return null

    val loopRange = KtPsiUtil.deparenthesize(PsiUtils.getLoopRange(d))
    var resolvedCall = CallUtilKt.getResolvedCall(loopRange, c.bindingContext())

    if (resolvedCall == null) return null

    var step: KtExpression = null
    if (DescriptorUtils.getFqNameSafe(resolvedCall.getResultingDescriptor) == stepFunctionName ) {
      step = resolvedCall.getCall.getValueArguments.get(0).getArgumentExpression
      if (step == null) return null

      resolvedCall = {
        val extRcv = resolvedCall.getExtensionReceiver

        if (extRcv != null) {
          extRcv match {
            case expRcv: ExpressionReceiver =>
              val expr = expRcv.getExpression
              if (expr != null) {
                CallUtilKt.getResolvedCall(expr, c.bindingContext())
              } else null
            case _ => null
          }
        } else null
      }

      if (resolvedCall == null) return null
    }

    val first = {
      val extRcv = resolvedCall.getExtensionReceiver

      val rcv = if (extRcv != null) {
        extRcv
      } else {
        resolvedCall.getDispatchReceiver
      }

      if (rcv == null) return null

      rcv match {
        case exprRcv: ExpressionReceiver =>
          exprRcv.getExpression
        case _ => null
      }
    }

    val second = {
      val valArgs = resolvedCall.getValueArgumentsByIndex
      if (valArgs == null || valArgs.isEmpty) return null

      val args = valArgs.get(0).getArguments

      if (args == null || args.isEmpty) return null

      args.get(0).getArgumentExpression
    }

    val resultingFqName: FqName = DescriptorUtils.getFqNameSafe(resolvedCall.getResultingDescriptor)
    if (resultingFqName == null) return null

    val tpe = resultingFqName.asString() match {
      case n if n == rangeToFunctionName.asString() => new RangeTo
      case n if n == untilFunctionName.asString() => new Until
      case n if n == downToFunctionName.asString() => new DownTo
      case _ => null
    }


    RangeLiteral(tpe, first, second, step)
  }

}
