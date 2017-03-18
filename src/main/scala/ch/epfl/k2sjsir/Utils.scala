package ch.epfl.k2sjsir

import org.jetbrains.kotlin.name.Name
import org.jetbrains.kotlin.types.KotlinType
import org.scalajs.core.ir.Types._

object Utils {

  implicit class TypeTranslator(kTpe: KotlinType) {
    def toJsType: Type = getType(kTpe.toString)
  }

  implicit class NameTranslator(kName: Name) {
    def toJsName: String = NameEncoder.encodeName(kName.asString())
  }

  private def getType(kTpe: String): Type = kTpe match {
    case "Boolean" => BooleanType
    case "Char" => IntType
    case "Byte" => IntType
    case "Short" => IntType
    case "Int" => IntType
    case "Float" => FloatType
    case "Long" => LongType
    case "Double" => DoubleType
    case "Any" => AnyType
    case "Null" => NullType
    case "Unit" => NoType
    case _ =>
      println(s"Check type found: $kTpe")
      ClassType(kTpe)
  }
}
