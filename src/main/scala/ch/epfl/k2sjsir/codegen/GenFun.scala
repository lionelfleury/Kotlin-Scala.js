package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.NameEncoder
import ch.epfl.k2sjsir.Utils._
import org.jetbrains.kotlin.descriptors.ClassDescriptor
import org.jetbrains.kotlin.ir.declarations._
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.NoType

import scala.collection.JavaConversions._
import scala.language.implicitConversions

case class GenFun(d: IrFunction, p: Positioner) extends Gen[IrFunction] {

  def tree: Tree = d match {
    case c: IrConstructor => genConstructor(c)
    case f: IrFunction => genFun(f)
    case _ => notImplemented
  }

  private def genFun(f: IrFunction): Tree = {
    val descriptor = d.getDescriptor
    val owner = descriptor.getContainingDeclaration match {
      case c: ClassDescriptor => c
      case _ => throw new Error("Function at package level are not yet supported...")
    }
    val name = NameEncoder.encodeMethodIdent(descriptor, owner)
    val args = for (p <- descriptor.getValueParameters) yield {
      val idt = Ident(p.getName.toJsName)
      val tpe = p.getType.toJsType
      ParamDef(idt, tpe, p.isVar, rest = false)
    }
    val tpe = descriptor.getReturnType.toJsType
    val body = Option(GenBody(d.getBody, p).tree)
    val opt = OptimizerHints.empty
    MethodDef(static = false, name, args.toList, tpe, body)(opt, None)
  }

  private def genConstructor(c: IrConstructor): Tree = {
    val descriptor = d.getDescriptor
    val owner = descriptor.getContainingDeclaration match {
      case c: ClassDescriptor => c
      case _ => throw new Error("Function at package level are not yet supported...")
    }
    val name = NameEncoder.encodeMethodIdent(descriptor, owner)
    val args = Nil
    val tpe = NoType
    val b = d.getBody
    val body = Option(GenBody(b, p).tree)
    val opt = OptimizerHints.empty
    val hash = None
    MethodDef(static = true, name, args, tpe, body)(opt, hash)
  }

}
