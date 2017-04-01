package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.ir.declarations._
import org.jetbrains.kotlin.resolve.DescriptorUtils
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types._

import scala.collection.JavaConverters._

case class GenFun(d: IrFunction, p: Positioner) extends Gen[IrFunction] {

  def tree: Tree = d match {
    case c: IrConstructor => genFun(c, NoType)
    case f: IrFunction => genFun(f, f.getDescriptor.getReturnType.toJsType)
    case _ => notImplemented
  }

  private def genFun(f: IrFunction, tpe: Type): Tree = {
    val desc = d.getDescriptor
    val body = GenBody(d.getBody, p).treeOption
    val idt = desc.toJsMethodIdent
    val x = Option(desc.getExtensionReceiverParameter).map(_.toJsParamDef)
    val args = x ++ desc.getValueParameters.asScala.map(_.toJsParamDef)
    val opt = OptimizerHints.empty.withInline(desc.isInline)
    val static = DescriptorUtils.isStaticDeclaration(desc)
    MethodDef(static, idt, args.toList, tpe, body)(opt, None)
  }

}
