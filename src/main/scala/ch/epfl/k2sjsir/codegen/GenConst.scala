package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.{Trees => js}
import scala.language.existentials

case class GenConst(d: IrConst[_], p: Positioner) extends Gen[IrConst[_]] {

  def tree: js.Tree = d.getValue match {
    case null => js.Null()
    case b: Boolean => js.BooleanLiteral(b)
    case c: Char => js.IntLiteral(c)
    case b: Byte => js.IntLiteral(b)
    case s: Short => js.IntLiteral(s)
    case i: Int => js.IntLiteral(i)
    case l: Long => js.LongLiteral(l)
    case s: String => js.StringLiteral(s)
    case f: Float => js.FloatLiteral(f)
    case d: Double => js.DoubleLiteral(d)
    case _ => notImplemented
  }

}
