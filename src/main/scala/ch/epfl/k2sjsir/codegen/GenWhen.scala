package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.expressions.impl.IrWhenBase
import org.jetbrains.kotlin.ir.expressions.{IrBranch, IrWhen}
import org.scalajs.core.ir.Trees.{Tree, _}

import scala.collection.JavaConversions._
import scala.language.implicitConversions

case class GenWhen(d: IrWhen, p: Positioner) extends Gen[IrWhen] {
  override def tree: Tree = d match {
    case i : IrWhenBase => generateIfs(i.getBranches.toList)
    case _ => notImplemented
  }

  private def generateIfs(branches: List[IrBranch]): Tree = {
    branches match {
      case List() => Skip()
      case branch :: branches =>
        val condExpr = GenExpr(branch.getCondition, p).tree
        val thenBranchExpr = GenExpr(branch.getResult, p).tree
        If(condExpr, thenBranchExpr, generateIfs(branches))(thenBranchExpr.tpe)
    }
  }


}
