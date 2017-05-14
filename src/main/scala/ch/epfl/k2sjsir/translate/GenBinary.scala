package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.operation.AssignmentTranslator
import org.jetbrains.kotlin.js.translate.operation.CompareToTranslator.isCompareToCall
import org.jetbrains.kotlin.js.translate.utils.BindingUtils.getCallableDescriptorForOperationExpression
import org.jetbrains.kotlin.js.translate.utils.PsiUtils.getOperationToken
import org.jetbrains.kotlin.lexer.{KtSingleValueToken, KtToken, KtTokens}
import org.jetbrains.kotlin.psi.KtBinaryExpression
import org.jetbrains.kotlin.types.expressions.OperatorConventions
import org.scalajs.core.ir.Position
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types._

case class GenBinary(d: KtBinaryExpression)(implicit val c: TranslationContext) extends Gen[KtBinaryExpression] {
  import GenBinary._

  override def tree: Tree = {
    val desc = getCallableDescriptorForOperationExpression(c.bindingContext(), d)
    val op = getOperationToken(d)
    val lhs = GenExpr(d.getLeft).tree
    val rhs = GenExpr(d.getRight).tree
    val tpe = if (desc != null) desc.getReturnType.toJsType else lhs.tpe

    if (op == KtTokens.ELVIS) translateElvis(lhs, rhs)
    else if (AssignmentTranslator.isAssignmentOperator(op)) GenAssign(d).tree
    else if (isNotOverloadable(op)) {
      val binOp = getBinaryOp(op.toString, lhs.tpe)
      BinaryOp(binOp, lhs, rhs)
    } else if (isCompareToCall(op, desc)) {
      if (isNumericType(lhs.tpe) && isNumericType(rhs.tpe)) {
        BinaryOp(numBinaryOp(op.toString), lhs, rhs)
      } else if (lhs.tpe == StringType || lhs.tpe == ClassType("T")) {
        val s = stringCompareTo(lhs, rhs)
        val binOp = numBinaryOp(op.toString)
        BinaryOp(binOp, s, IntLiteral(0))
      } else notImplemented
    } else if (isEquals(op)) {
      val binOp =
        if (isLongType(lhs.tpe) && isLongType(rhs.tpe)) longBinaryOp(op.toString)
        else if (isNumericType(lhs.tpe) && isNumericType(rhs.tpe)) numBinaryOp(op.toString)
        else builtinBinarOp(op.toString)
      BinaryOp(binOp, lhs, rhs)
    } else {
      val binOp = op match {
        case KtTokens.IDENTIFIER => getBinaryOp(desc.toJsName, tpe)
        case k: KtSingleValueToken => getBinaryOp(k.toString, tpe)
        case _ => notImplemented; -1
      }
      val (clhs, crhs) = if(isLongOp(binOp, lhs.tpe, rhs.tpe)) {
        (intToLong(lhs), if (isLongSpecial(binOp)) longToInt(rhs) else intToLong(rhs))
      } else {
        val lsrc = convertArg(binOp, lhs, lhs.tpe, tpe)
        val rsrc = convertArg(binOp, rhs, rhs.tpe, tpe)
        (lsrc, rsrc)
      }
      BinaryOp(binOp, clhs, crhs)
    }
  }

  private def translateElvis(left: Tree, right: Tree): Tree = {
    notImplemented
  }

  private def isNotOverloadable(op: KtToken): Boolean =
    OperatorConventions.NOT_OVERLOADABLE.contains(op)

  private def isEquals(op: KtToken): Boolean =
    (op == KtTokens.EQEQ) || (op == KtTokens.EXCLEQ)

}

object GenBinary {

  private def isLongType(t: Type) = t == LongType
  private def isStringType(t: Type) = t == StringType
  private def isFloatType(t: Type) = t == FloatType
  private def isIntType(t: Type) = t == IntType
  private def isDoubleType(t: Type) = t == DoubleType

  /* Long is special, because of its shift operations, that accepts only int */
  private def isLongOp(op: BinaryOp.Code, ltpe: Type, rtpe: Type) = {
    (isLongType(ltpe) || isLongType(rtpe)) &&
      !(isFloatType(ltpe) || isFloatType(rtpe) || isStringType(ltpe) ||
        isStringType(rtpe) || isDoubleType(ltpe) || isDoubleType(rtpe))

  }

  private def isLongSpecial(op: BinaryOp.Code): Boolean =
    op == BinaryOp.Long_<< || op == BinaryOp.Long_>> || op == BinaryOp.Long_>>>

  private def intToLong(t: Tree)(implicit p: Position) = if (isLongType(t.tpe)) t else UnaryOp(UnaryOp.IntToLong, t)

  private def longToInt(t: Tree)(implicit p: Position) = if (isIntType(t.tpe)) t else UnaryOp(UnaryOp.LongToInt, t)

  /* Convert arg to the correct type for it to be us by the binary op */
  private def convertArg(op: BinaryOp.Code, tree: Tree, t: Type, returnType: Type)(implicit p: Position) = {
    val notLong = {
      if (!isLongType(t)) tree
      else if (isLongSpecial(op)) UnaryOp(UnaryOp.LongToInt, tree)
      else UnaryOp(UnaryOp.LongToDouble, tree)
    }
    if (!isFloatType(returnType)) notLong
    else if (isFloatType(t)) notLong
    else UnaryOp(UnaryOp.DoubleToFloat, notLong)
  }

  private val numBinaryOp = Map(
    "LT" -> BinaryOp.Num_<,
    "LTE" -> BinaryOp.Num_<=,
    "EQEQ" -> BinaryOp.Num_==,
    "GTE" -> BinaryOp.Num_>=,
    "GT" -> BinaryOp.Num_>,
    "EXCLEQ" -> BinaryOp.Num_!=
  )

  private val longBinaryOp = Map(
    "EQEQ" -> BinaryOp.Long_==,
    "EXCLEQ" -> BinaryOp.Long_!=,
    "PLUS" -> BinaryOp.Long_+,
    "MINUS" -> BinaryOp.Long_-,
    "MUL" -> BinaryOp.Long_*,
    "DIV" -> BinaryOp.Long_/,
    "PERC" -> BinaryOp.Long_%,
    "or" -> BinaryOp.Long_|,
    "and" -> BinaryOp.Long_&,
    "xor" -> BinaryOp.Long_^,
    "shl" -> BinaryOp.Long_<<,
    "shr" -> BinaryOp.Long_>>,
    "ushr" -> BinaryOp.Long_>>>,
    "inc" -> BinaryOp.Long_+,
    "dec" -> BinaryOp.Long_-
  )

  private val intBinaryOp = Map(
    "PLUS" -> BinaryOp.Int_+,
    "MINUS" -> BinaryOp.Int_-,
    "MUL" -> BinaryOp.Int_*,
    "DIV" -> BinaryOp.Int_/,
    "PERC" -> BinaryOp.Int_%,
    "or" -> BinaryOp.Int_|,
    "and" -> BinaryOp.Int_&,
    "xor" -> BinaryOp.Int_^,
    "shl" -> BinaryOp.Int_<<,
    "shr" -> BinaryOp.Int_>>,
    "ushr" -> BinaryOp.Int_>>>,
    "inc" -> BinaryOp.Int_+,
    "dec" -> BinaryOp.Int_-
  )

  private val doubleBinaryOp = Map(
    "PLUS" -> BinaryOp.Double_+,
    "MINUS" -> BinaryOp.Double_-,
    "MUL" -> BinaryOp.Double_*,
    "DIV" -> BinaryOp.Double_/,
    "PERC" -> BinaryOp.Double_%,
    "inc" -> BinaryOp.Double_+,
    "dec" -> BinaryOp.Double_-
  )

  private val floatBinaryOp = Map(
    "PLUS" -> BinaryOp.Float_+,
    "MINUS" -> BinaryOp.Float_-,
    "MUL" -> BinaryOp.Float_*,
    "DIV" -> BinaryOp.Float_/,
    "PERC" -> BinaryOp.Float_%,
    "inc" -> BinaryOp.Float_+,
    "dec" -> BinaryOp.Float_-
  )

  private val booleanBinaryOp = Map(
    "EQEQ" -> BinaryOp.Boolean_==,
    "EXCLEQ" -> BinaryOp.Boolean_!=,
    "or" -> BinaryOp.Boolean_|,
    "and" -> BinaryOp.Boolean_&
  )

  private val stringBinaryOp = Map(
    "PLUS" -> BinaryOp.String_+
  )

  private val builtinBinarOp = Map(
    "EQEQ" -> BinaryOp.===,
    "EXCLEQ" -> BinaryOp.!==
  )

  private def opMap(tpe: Type): Map[String, Int] = tpe match {
    case BooleanType => booleanBinaryOp
    case IntType => intBinaryOp
    case LongType => longBinaryOp
    case FloatType => floatBinaryOp
    case DoubleType => doubleBinaryOp
    case StringType | ClassType("T") => stringBinaryOp
    case _ => builtinBinarOp
  }

  /* Find the correct binary op for a given type */
  def getBinaryOp(op: String, tpe: Type): BinaryOp.Code =
    opMap(tpe).getOrElse(op, throw new Error(s"Binary op not found: $op for type: $tpe"))

  val isNumericType: Type => Boolean =
    Set(IntType, LongType, DoubleType, FloatType)

  private def stringCompareTo(lhs: Tree, rhs: Tree)(implicit pos: Position): Tree =
    Apply(LoadModule(ClassType("sjsr_RuntimeString$")),
          Ident("compareTo__T__T__I", Some("compareTo__T__T__I")),
          List(lhs, rhs))(IntType)

}
