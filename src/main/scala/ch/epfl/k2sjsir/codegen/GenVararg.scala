package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.ArrayType

import scala.collection.JavaConverters._

case class GenVararg(d: IrVararg, p: Positioner) extends Gen[IrVararg] {

  def tree: Tree = {
    val tpe = d.getVarargElementType.toJsRefType
    val args = d.getElements.asScala.map {
      case x: IrConst[_] => GenConst(x, p).tree
      case _ => notImplemented
    }
    ArrayValue(ArrayType(tpe), args.toList)
  }

}
