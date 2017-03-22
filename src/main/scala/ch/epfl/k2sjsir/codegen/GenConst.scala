package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.expressions.IrConst
import org.scalajs.core.ir.Trees._

case class GenConst(d: IrConst[_], p: Positioner) extends Gen[IrConst[_]] {

  def tree: Tree = d.getValue match {
    case null => Null()
    case b: Boolean => BooleanLiteral(b)
    case c: Char => IntLiteral(c)
    case b: Byte => IntLiteral(b)
    case s: Short => IntLiteral(s)
    case i: Int => IntLiteral(i)
    case l: Long => LongLiteral(l)
    case s: String => StringLiteral(s)
    case f: Float => FloatLiteral(f)
    case d: Double => DoubleLiteral(d)
    case _ => notImplemented
  }

}
