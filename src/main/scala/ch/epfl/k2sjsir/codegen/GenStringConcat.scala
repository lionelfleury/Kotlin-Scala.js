package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.expressions.IrStringConcatenation
import org.scalajs.core.ir.Trees._
import scala.collection.JavaConverters._

case class GenStringConcat(d: IrStringConcatenation, p: Positioner) extends Gen[IrStringConcatenation] {

  def tree: Tree = {
    val args = d.getArguments.asScala.map(GenExpr(_, p).tree)
    args.foldLeft(StringLiteral("") : Tree)((n, acc : Tree) => BinaryOp(BinaryOp.String_+, n, acc))
  }

}
