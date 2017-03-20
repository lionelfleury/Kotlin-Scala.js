package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.IrElement
import org.scalajs.core.ir.Position
import org.scalajs.core.ir.Trees.{Skip, Tree}

import scala.language.implicitConversions

trait Gen[T <: IrElement] {
  def d: T
  def p: Positioner
  implicit def pos: Position = p.getPos(d.getStartOffset)
  def tree: Tree

  def notImplemented: Tree = {
    val c = getClass.getSimpleName
    val name = if (d != null) d.getClass.getSimpleName else ""
    println(s"Not supported $c: $name")
    Skip()
  }

}
