package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.ir.declarations._
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.NoType

import scala.collection.JavaConverters._

case class GenFun(d: IrFunction, p: Positioner) extends Gen[IrFunction] {

  def tree: Tree = d match {
    case c: IrConstructor =>
      val args = c.getDescriptor.getValueParameters.asScala.map(_.toJsParamDef)
      val idt = c.getDescriptor.toJsMethodIdent
      val body = GenBody(d.getBody, p).treeOption
      val opt = OptimizerHints.empty
      MethodDef(static = false, idt, args.toList, NoType, body)(opt, None)
    case f: IrFunction => genFun(f)
    case _ => notImplemented
  }

  private def genFun(f: IrFunction): Tree = {
    val desc = d.getDescriptor
    val tpe = desc.getReturnType.toJsType
    val body = GenBody(d.getBody, p).treeOption
    val idt = desc.toJsMethodIdent
    val args = desc.getValueParameters.asScala.map(_.toJsParamDef)
    val opt = OptimizerHints.empty
    MethodDef(static = false, idt, args.toList, tpe, body)(opt, None)
  }

}
