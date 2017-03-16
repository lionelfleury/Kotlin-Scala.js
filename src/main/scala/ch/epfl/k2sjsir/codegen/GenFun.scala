package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.NameEncoder
import org.jetbrains.kotlin.descriptors.ClassDescriptor
import org.jetbrains.kotlin.ir.declarations._
import org.scalajs.core.ir.Trees._

case class GenFun(d: IrFunction, p: Positioner)
  extends AbstractGenerator(d, p) {

  def generate(): Tree = d match {
    case _: IrConstructor => genConstructor()
    case _ => genConstructor()
  }

  private def genConstructor(): Tree = {
    val descriptor = d.getDescriptor
    val owner = descriptor.getContainingDeclaration.asInstanceOf[ClassDescriptor].getName.asString()
    val methodName = NameEncoder.encodeMethodIdent(descriptor, owner)
    val args = Nil
    val resultType = descriptor.getReturnType.toJsType
    val b = d.getBody
    val body = if (b == null) None else Some(GenBody(b, p).generate())
    val opt = OptimizerHints.empty
    val hash = None
    MethodDef(static = false, methodName, args, resultType, body)(opt, hash)
  }

}
