package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.IrElement
import org.jetbrains.kotlin.types.KotlinType
import org.scalajs.core.ir.Position
import org.scalajs.core.ir.Trees.{Skip, Tree}
import org.scalajs.core.ir.Types._

import scala.language.implicitConversions

trait Gen[T <: IrElement] {
  def d: T
  def p: Positioner
  implicit def pos: Position = p.getPos(d.getStartOffset)
  def tree: Tree

  def notImplemented: Tree = {
    val name = d.getClass.getSimpleName
    val c = getClass.getSimpleName
    println(s"Not supported $c: $name")
    Skip()
  }

  //TODO: Move outside in other object/class
  implicit class TypeTranslator(kTpe: KotlinType) {
    def toJsType: Type = getType(kTpe.toString)
  }

  //TODO: Move outside in other object/class
  protected def getType(kTpe: String): Type = kTpe match {
    case "Boolean" => BooleanType
    //    case CHAR => jstpe.Char
    //    case BYTE => jstpe.Byte
    //    case SHORT => jstpe.SHORT
    case "Int" => IntType
    case "Float" => FloatType
    case "Long" => LongType
    case "Double" => DoubleType
    case "Any" => AnyType
    case _ =>
      println(s"Check type found: $kTpe")
      ClassType(kTpe)
  }

}
