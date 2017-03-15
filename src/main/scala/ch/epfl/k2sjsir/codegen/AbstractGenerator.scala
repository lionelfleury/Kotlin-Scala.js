package ch.epfl.k2sjsir.codegen

import org.jetbrains.kotlin.ir.IrElement
import org.jetbrains.kotlin.types.KotlinType
import org.scalajs.core.ir.Position
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types._

import scala.language.implicitConversions

abstract class AbstractGenerator(d: IrElement, p: Positioner) {

  implicit protected def pos: Position = p.getPos(d.getStartOffset)

  def generate(): Tree

  implicit class TypeTranslator(kTpe: KotlinType) {
    def toJsType: Type = getType(kTpe.toString)
  }

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
      println(s"No corresponding type found: $kTpe")
      NoType
  }

}
