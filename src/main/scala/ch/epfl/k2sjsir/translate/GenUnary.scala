package ch.epfl.k2sjsir.translate

import com.intellij.psi.tree.IElementType
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.psi.{KtPostfixExpression, KtPrefixExpression, KtUnaryExpression}
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types._
import org.scalajs.core.ir.{Position, Trees}

case class GenUnary(d: KtUnaryExpression)(implicit val c: TranslationContext) extends Gen[KtUnaryExpression] {
  import GenUnary._

  override def tree: Trees.Tree = {
    val lhs = GenExpr(d.getBaseExpression).tree
    val binOp = getBinaryOp(d.getOperationToken, lhs)
    d match {
      case _: KtPrefixExpression =>
        Block(Assign(lhs, binOp), lhs)
      case _: KtPostfixExpression =>
        val v = VarDef(Ident("tmp$1"), lhs.tpe, mutable = false, lhs)
        Block(v, Assign(lhs, binOp), v.ref)
      case _ => notImplemented
    }
  }

}

object GenUnary {

  private def getBinaryOp(op: IElementType, lhs: Tree)(implicit pos: Position) = {
    val binOp = op match {
      case KtTokens.PLUSPLUS => "PLUS"
      case KtTokens.MINUSMINUS => "MINUS"
      case _ => sys.error("Not found UnaryOp!")
    }
    val lit = lhs.tpe match {
      case IntType => IntLiteral(1)
      case LongType => LongLiteral(1L)
      case DoubleType => DoubleLiteral(1d)
      case FloatType => FloatLiteral(1f)
    }
    BinaryOp(GenBinary.getBinaryOp(binOp, lhs.tpe), lhs, lit)
  }

  /* Useful for explicit type conversion (toInt, toDouble, ...) */
  def convertToOp(from: Type, to: Type) = (from, to) match {
    case (IntType, LongType) => Some(UnaryOp.IntToLong)
    case (LongType, IntType) => Some(UnaryOp.LongToInt)
    case (LongType, DoubleType) => Some(UnaryOp.LongToDouble)
    case (DoubleType, IntType) => Some(UnaryOp.DoubleToInt)
    case (DoubleType, FloatType) => Some(UnaryOp.DoubleToFloat)
    case (DoubleType, LongType) => Some(UnaryOp.DoubleToLong)
    case (BooleanType, BooleanType) => Some(UnaryOp.Boolean_!)
    case _ => None
  }

  def isUnaryOp(n: String): Boolean =
    n == "toLong" || n == "toInt" || n == "toDouble" || n == "toFloat" || n == "not" || isUnaryToBinary(n)

  private def isUnaryToBinary(n: String) = n == "unaryMinus"

}
