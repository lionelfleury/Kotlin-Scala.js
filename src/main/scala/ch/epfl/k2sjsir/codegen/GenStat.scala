package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.NameEncoder
import org.jetbrains.kotlin.ir.IrStatement
import org.jetbrains.kotlin.ir.declarations._
import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.{Types, Trees => js}

case class GenStat(d: IrStatement, p: Positioner) extends Gen[IrStatement] {

  def tree: js.Tree = d match {
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
    case c: IrInstanceInitializerCall =>
      val classDescriptor = c.getClassDescriptor
      val tpe = GenClass.getClassType(classDescriptor)
      val this_ = js.This()(Types.NoType)
      js.StoreModule(tpe, this_)
    case b: IrBlock => GenBlock(b, p).tree
    case f: IrFunction => GenFun(f, p).tree
    case r: IrReturn => GenReturn(r, p).tree
    case s: IrSetField => GenSetField(s, p).tree
    case _ => notImplemented
  }

}
