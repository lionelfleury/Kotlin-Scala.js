package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.expressions.{IrDelegatingConstructorCall, IrExpression, IrVararg}
import org.scalajs.core.ir.Trees.{ApplyStatically, This, Tree}
import org.scalajs.core.ir.Types.NoType

import scala.collection.JavaConverters._
import ch.epfl.k2sjsir.utils.Utils._

case class GenSuperCall(d: IrDelegatingConstructorCall, p: Positioner) extends Gen[IrDelegatingConstructorCall] {

  def tree: Tree =  {
    val cd = d.getDescriptor
    val tpe = cd.getConstructedClass.toJsClassType
    val args = genArgs(cd.getValueParameters.asScala.indices)
    ApplyStatically(This()(tpe), tpe, cd.toJsMethodIdent, args.toList)(NoType)
  }

  private def genArgs(as: Range): Seq[Tree] = as.map(d.getValueArgument).map {
    case a: IrVararg => GenVararg(a, p).tree
    case a: IrExpression => GenExpr(a, p).tree
    case _ => notImplemented
  }
}
