package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.expressions.impl.IrWhenBase
import org.jetbrains.kotlin.ir.expressions.{IrBranch, IrWhen}
import org.scalajs.core.ir.Trees.{Tree, _}

import scala.collection.JavaConverters._

case class GenWhen(d: IrWhen, p: Positioner) extends Gen[IrWhen] {

  override def tree: Tree = d match {
    case i: IrWhenBase => generateIfs(i.getBranches.asScala.toList)
    case _ => notImplemented
  }

  private def generateIfs(branches: List[IrBranch]): Tree = {
    branches match {
      case List() => Skip()
      case b :: bs =>
        val cond = GenExpr(b.getCondition, p).tree
        val thenB = GenExpr(b.getResult, p).tree
        If(cond, thenB, generateIfs(bs))(thenB.tpe)
    }
  }

}
