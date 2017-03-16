package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.NameEncoder
import org.jetbrains.kotlin.ir.IrStatement
import org.jetbrains.kotlin.ir.declarations._
import org.jetbrains.kotlin.ir.expressions.{IrBlock, IrDelegatingConstructorCall, IrInstanceInitializerCall}
import org.scalajs.core.ir.Trees.Skip
import org.scalajs.core.ir.{Types, Trees => js}

case class GenStat[T <: IrStatement](d: T, p: Positioner) extends AbstractGenerator[T](d, p) {

  def generate(): js.Tree = d match {
    case c: IrDelegatingConstructorCall =>
      val this_ = js.This()(Types.NoType)
      val method = c.getDescriptor
      val classDescriptor = method.getConstructedClass
      val classTpe = GenClass.getClassType(classDescriptor)
      val name = NameEncoder.encodeMethodIdent(method, classDescriptor)
      // TODO: look for args...
      val args = Nil
      val tpe = Types.NoType
      js.ApplyStatically(this_, classTpe, name, args)(tpe)
    case b: IrBlock => GenBlock(b, p).generate()
    case f: IrFunction => GenFun(f, p).generate()
    case c: IrInstanceInitializerCall =>
      val classDescriptor = c.getClassDescriptor
      val tpe = GenClass.getClassType(classDescriptor)
      val this_ = js.This()(Types.NoType)
      js.StoreModule(tpe, this_)
    case _ =>
      println(s"Not supported Statement: $d")
      Skip()
  }

}
