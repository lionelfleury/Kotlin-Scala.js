package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.utils.Utils
import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.operation.AssignmentTranslator
import org.jetbrains.kotlin.js.translate.operation.CompareToTranslator.isCompareToCall
import org.jetbrains.kotlin.js.translate.utils.BindingUtils
import org.jetbrains.kotlin.js.translate.utils.BindingUtils.getCallableDescriptorForOperationExpression
import org.jetbrains.kotlin.js.translate.utils.PsiUtils.getOperationToken
import org.jetbrains.kotlin.lexer.{KtSingleValueToken, KtToken, KtTokens}
import org.jetbrains.kotlin.psi.KtBinaryExpression
import org.jetbrains.kotlin.types.{KotlinType, TypeUtils}
import org.jetbrains.kotlin.types.expressions.OperatorConventions
import org.scalajs.core.ir.Position
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types._
import org.scalajs.core.ir.Trees.BinaryOp._

import scala.annotation.switch

case class GenBinary(d: KtBinaryExpression)(implicit val c: TranslationContext) extends Gen[KtBinaryExpression] {
  import GenBinary._

  val desc = getCallableDescriptorForOperationExpression(c.bindingContext(), d)
  val op = getOperationToken(d)
  val lhs = GenExpr(d.getLeft).tree
  val rhs = GenExpr(d.getRight).tree
  val tpe = if (desc != null) desc.getReturnType.toJsType else lhs.tpe

  override def tree: Tree = {

    if (op == KtTokens.ELVIS) translateElvis(d)
    else if(op == KtTokens.ANDAND) If(lhs, rhs, BooleanLiteral(false))(BooleanType)
    else if(op == KtTokens.OROR) If(lhs, BooleanLiteral(true), rhs)(BooleanType)
    else if (AssignmentTranslator.isAssignmentOperator(op)) GenAssign(d).tree
    else if (isNotOverloadable(op)) {
      val binOp = getBinaryOp(op.toString, lhs.tpe)
      BinaryOp(binOp, lhs, rhs)
    } else if (isRefCompare(op)) {
      val binOp = builtinBinarOp(op.toString)
      BinaryOp(binOp, lhs, rhs)
    }
    else if (isCompareToCall(op, desc)) {

      if (isNumericType(lhs.tpe) && isNumericType(rhs.tpe)) {
        BinaryOp(doubleBinaryOp(op.toString), adaptPrimitive(lhs, DoubleType), adaptPrimitive(rhs, DoubleType))
      } else if (lhs.tpe == StringType || lhs.tpe == ClassType("T")) {
        val s = stringCompareTo(lhs, rhs)
        val binOp = intBinaryOp(op.toString)
        BinaryOp(binOp, s, IntLiteral(0))
      } else notImplemented()

    } else if (tpe == StringType || tpe == ClassType("T")) {
      BinaryOp(String_+, lhs, rhs)
    } else {

      val opName = op match {
        case KtTokens.IDENTIFIER => desc.getName.asString()
        case k: KtSingleValueToken => k.toString
        case _ => op.toString
      }

      // See https://github.com/scala-js/scala-js/blob/master/compiler/src/main/scala/org/scalajs/core/compiler/GenJSCode.scala -> def genSimpleOp
      val isShift = isShiftOp(opName)
      val leftKind = lhs.tpe
      val rightKind = rhs.tpe

      val opType = {
        if (isShift) {
          if (leftKind == LongType) LongType
          else IntType
        } else {
          (leftKind, rightKind) match {
            case (DoubleType, _) | (_, DoubleType) => DoubleType
            case (FloatType, _) | (_, FloatType) => FloatType
            case (LongType, _) | (_, LongType) => LongType
            case (CharType|ByteType|ShortType|IntType, _) | (_, CharType|ByteType|ShortType|IntType) => IntType
            case (BooleanType, _) | (_, BooleanType) => BooleanType
            case _ => AnyType
          }
        }
      }

      val lsrc =
        if (opType == AnyType || opType == StringType) lhs
        else adaptPrimitive(lhs, opType)
      val rsrc =
        if (opType == AnyType || opType == StringType) rhs
        else adaptPrimitive(rhs, if (isShift) IntType else opType)

      (opType: @unchecked) match {
        case IntType =>
          BinaryOp(intBinaryOp(opName), lsrc, rsrc)

        case LongType =>
          BinaryOp(longBinaryOp(opName), lsrc, rsrc)

        case FloatType =>
          def withFloats(op: Int): Tree =
            BinaryOp(op, lsrc, rsrc)

          def toDouble(value: Tree): Tree =
            UnaryOp(UnaryOp.FloatToDouble, value)

          def withDoubles(op: Int): Tree =
            BinaryOp(op, toDouble(lsrc), toDouble(rsrc))

          (opName) match {
            case "PLUS" => withFloats(Float_+)
            case "MINUS" => withFloats(Float_-)
            case "MUL" => withFloats(Float_*)
            case "DIV" => withFloats(Float_/)
            case "PERC" => withFloats(Float_%)

            case "EQEQ"  => withDoubles(Double_==)
            case "EXCLEQ"  => withDoubles(Double_!=)
            case "LT"  => withDoubles(Double_<)
            case "LTEQ"  => withDoubles(Double_<=)
            case "GT"  => withDoubles(Double_>)
            case "GTEQ"  => withDoubles(Double_>=)
          }

        case DoubleType =>
          BinaryOp(doubleBinaryOp(opName), lsrc, rsrc)

        case BooleanType =>
          (opName: @switch) match {
            case "or" =>
              BinaryOp(Boolean_|, lsrc, rsrc)
            case "and" =>
              BinaryOp(Boolean_&, lsrc, rsrc)
            case "EQEQ" =>
              BinaryOp(Boolean_==, lsrc, rsrc)
            case "xor" | "EXCLEQ" =>
              BinaryOp(Boolean_!=, lsrc, rsrc)
          }

        case AnyType => {
          // See structural equality definition in Kotlin docs
          val block = Utils.genStructuralEq(lhs, rhs)

          if (op == KtTokens.EXCLEQ)
            UnaryOp(UnaryOp.Boolean_!, block)
          else
            block
        }
      }
    }
  }

  private def translateElvis(expr: KtBinaryExpression): Tree = {
    val left = expr.getLeft
    val right = expr.getRight

    val exprTpe = BindingUtils.getTypeForExpression(c.bindingContext(), expr)

    val lhs = GenExpr(left).tree
    val rhs = GenExpr(right).tree

    val freshVar = VarDef(Ident(Utils.getFreshName()), lhs.tpe, mutable = false, lhs)

    val cond = genNotNullCond(freshVar.ref)

    // lhs is always nullable (because of elvis), therefore we need to cast it
    // it's safe to do so because it will only be cast if it's non null
    Block(freshVar, If(cond, cast(freshVar.ref, exprTpe), rhs)(exprTpe.toJsType))
  }

  private def isNotOverloadable(op: KtToken): Boolean =
    OperatorConventions.NOT_OVERLOADABLE.contains(op)

  private def isStructCompare(op: KtToken): Boolean =
    (op == KtTokens.EQEQ) || (op == KtTokens.EXCLEQ)

  private def isRefCompare(op: KtToken): Boolean =
    (op == KtTokens.EQEQEQ) || (op == KtTokens.EXCLEQEQEQ)
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
    "dec" -> BinaryOp.Int_-,
    "LT" -> BinaryOp.Int_<,
    "LTEQ" -> BinaryOp.Int_<=,
    "EQEQ" -> BinaryOp.Int_==,
    "GTEQ" -> BinaryOp.Int_>=,
    "GT" -> BinaryOp.Int_>,
    "EXCLEQ" -> BinaryOp.Int_!=
  )

  private val doubleBinaryOp = Map(
    "PLUS" -> BinaryOp.Double_+,
    "MINUS" -> BinaryOp.Double_-,
    "MUL" -> BinaryOp.Double_*,
    "DIV" -> BinaryOp.Double_/,
    "PERC" -> BinaryOp.Double_%,
    "inc" -> BinaryOp.Double_+,
    "dec" -> BinaryOp.Double_-,
    "LT" -> BinaryOp.Double_<,
    "LTEQ" -> BinaryOp.Double_<=,
    "EQEQ" -> BinaryOp.Double_==,
    "GTEQ" -> BinaryOp.Double_>=,
    "GT" -> BinaryOp.Double_>,
    "EXCLEQ" -> BinaryOp.Double_!=
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
    "EQEQEQ" -> BinaryOp.===,
    "EXCLEQEQEQ" -> BinaryOp.!==
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

  private val shiftOps = Set("shl", "shr", "ushr"/*, "ushl"*/)

  private def isShiftOp(opName: String): Boolean = {
    shiftOps.contains(opName)
  }

  private def stringCompareTo(lhs: Tree, rhs: Tree)(implicit pos: Position): Tree =
    Apply(LoadModule(ClassType("sjsr_RuntimeString$")),
          Ident("compareTo__T__T__I", Some("compareTo__T__T__I")),
          List(lhs, rhs))(IntType)

}
