package ch.epfl.k2sjsir

import ch.epfl.k2sjsir.NameEncoder._
import org.jetbrains.kotlin.descriptors._
import org.jetbrains.kotlin.types.KotlinType
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types._
import org.scalajs.core.ir.{Definitions, Position}

object Utils {

  implicit class DeclarationDescriptorTranslator(d: DeclarationDescriptor) {
    def toJsName: String = encodeName(d.getName.asString())
    def toIdent(implicit pos: Position): Ident = Ident(d.toJsName, Some(d.getName.asString()))
  }

  implicit class CallableDescriptorTranslator(desc: CallableDescriptor) {
    def toMethodIdent(implicit pos: Position): Ident = encodeMethodIdent(desc)
  }

  implicit class ClassDescriptorTranslator(d: ClassDescriptor) {
    def toClassName: String = encodeClassFullName(d)
    def toClassIdent(implicit pos: Position): Ident = encodeClassFullNameIdent(d)
    def toJsClassType: ClassType = ClassType(d.toClassName)
  }

  implicit class ValueParameterTranslator(d: ValueParameterDescriptor) {
    def toVarRef(implicit pos: Position): VarRef = VarRef(d.toIdent)(d.getType.toJsType)
    def toParamDef(implicit pos: Position): ParamDef = ParamDef(d.toIdent, d.getType.toJsType, d.isVar, rest = false)
  }

  implicit class KotlinTypeTranslator(t: KotlinType) {
    def toJsType: Type = getType(t.toString)
    def toJsInternal: String = getInternal(t.toJsType)
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
    case "Nothing" => NothingType
    case _ =>
      println(s"Sould be class type: $kTpe")
      NoType
  }

  private def getInternal(tpe: Type): String = tpe match {
    case NothingType => Definitions.RuntimeNothingClass
    case NullType => Definitions.RuntimeNullClass
    case AnyType => "O"
    case ArrayType(elem, _) => "A" + encodeName(elem.toString)
    case other => primitiveCharCode(other)
  }

  private def primitiveCharCode(tpe: Type): String = tpe match {
    case NoType | NothingType => "V" // TODO: Check if really a good idea...
    case BooleanType => "Z"
    //    case CharClass     => 'C'
    //    case ByteClass     => 'B'
    //    case ShortClass    => 'S'
    case IntType => "I"
    case LongType => "J"
    case FloatType => "F"
    case DoubleType => "D"
    case _ => throw new Error(s"Unknown primitive type: $tpe")
  }

}
