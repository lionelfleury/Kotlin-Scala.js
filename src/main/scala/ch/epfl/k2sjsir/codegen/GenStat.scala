package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.NameEncoder
import org.jetbrains.kotlin.ir.IrStatement
import org.jetbrains.kotlin.ir.declarations._
import org.jetbrains.kotlin.ir.expressions.IrDelegatingConstructorCall
import org.scalajs.core.ir.Trees.Skip
import org.scalajs.core.ir.{Types, Trees => js}

case class GenStat[T <: IrStatement](d: T, p: Positioner) extends AbstractGenerator[T](d, p) {

  def generate(): js.Tree = d match {
    case _: IrDelegatingConstructorCall =>
      val this_ = js.This()(Types.NoType)
      val method = d.asInstanceOf[IrDelegatingConstructorCall].getDescriptor
      val classDescriptor = method.getConstructedClass
      val className = GenClass.getClassNameIdent(classDescriptor, p.getPos(d.getStartOffset)).name
      val name = NameEncoder.encodeMethodIdent(method, className)
      // TODO: look for args...
      val args = Nil
      val tpe = Types.NoType
      js.ApplyStatically(this_, Types.ClassType(className), name, args)(tpe)
    case _: IrField =>
      Skip() //GenField(d, p).generate()
    case _: IrFunction =>
      GenFun(d.asInstanceOf[IrFunction], p).generate()
    case _ =>
      println(s"Not supported Statement: $d")
      Skip()
  }

}
