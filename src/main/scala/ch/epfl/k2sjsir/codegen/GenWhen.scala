package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.expressions.impl.IrElseBranchImpl
import org.jetbrains.kotlin.ir.expressions.{IrBranch, IrWhen}
import org.scalajs.core.ir.Trees._

import scala.collection.JavaConverters._

case class GenWhen(d: IrWhen, p: Positioner) extends Gen[IrWhen] {

  override def tree: Tree = generateIfs(d.getBranches.asScala)

  private def generateIfs(bs: Seq[IrBranch]): Tree = bs match {
    case Seq(h: IrElseBranchImpl) =>
      GenExpr(h.getResult, p).tree
    case h +: t =>
      val cond = GenExpr(h.getCondition, p).tree
      val thenB = GenExpr(h.getResult, p).tree
      val elseB = if (t.nonEmpty) generateIfs(t) else Skip()
      If(cond, thenB, elseB)(thenB.tpe)
    case _ => notImplemented
  }

}
