package ch.epfl.k2sjsir.translate

import com.intellij.psi.tree.IElementType
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.psi.{KtPostfixExpression, KtPrefixExpression, KtUnaryExpression}
import org.scalajs.core.ir.Trees._
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
        val v = VarDef(Ident("tmp$1"), lhs.tpe, mutable = true, lhs)
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
    BinaryOp(GenBinary.getBinaryOp(binOp, lhs.tpe), lhs, IntLiteral(1))
  }

}
