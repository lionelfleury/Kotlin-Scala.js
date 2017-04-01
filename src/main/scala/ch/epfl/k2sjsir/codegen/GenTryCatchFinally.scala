package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.Trees._

import scala.collection.JavaConverters._


case class GenTryCatchFinally(d: IrTry, p: Positioner) extends Gen[IrTry] {

  def tree: Tree = {
    val r = GenExpr(d.getTryResult, p).tree
    val cs = d.getCatches.asScala.map(c => (c.getParameter.toJsVarRef, GenExpr(c.getResult, p).tree))
    val f = Option(d.getFinallyExpression).map(GenExpr(_, p).tree)
    val tc = cs.foldLeft(r) { case (b, (c, h)) => TryCatch(b, c.ident, h)(b.tpe) }
    if (f.nonEmpty) TryFinally(tc, f.get) else tc
  }

}
