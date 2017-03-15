package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.NameEncoder
import org.jetbrains.kotlin.descriptors.ClassDescriptor
import org.jetbrains.kotlin.ir.declarations._
import org.scalajs.core.ir.Trees._

case class GenFun(irFunction: IrFunction, p: Positioner)
  extends AbstractGenerator(irFunction, p) {

  def generate(): Tree = irFunction match {
    case _: IrConstructor => genConstructor()
    case _ => genConstructor()
  }

  private def genConstructor(): Tree = {
    val descriptor = irFunction.getDescriptor
    val owner = descriptor.getContainingDeclaration.asInstanceOf[ClassDescriptor].getName.asString()
    val methodName = NameEncoder.encodeMethodIdent(descriptor, owner)
    val args = Nil
    val resultType = descriptor.getReturnType.toJsType
    val body = GenBody(irFunction.getBody, p).generate()
    val opt = OptimizerHints.empty
    val hash = None
    MethodDef(static = false, methodName, args, resultType, Some(body))(opt, hash)
  }

}
