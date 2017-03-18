package ch.epfl.k2sjsir

import ch.epfl.k2sjsir.NameEncoder._
import org.jetbrains.kotlin.descriptors.{ClassDescriptor, DeclarationDescriptor, FunctionDescriptor, ValueParameterDescriptor}
import org.jetbrains.kotlin.types.KotlinType
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types._
import org.scalajs.core.ir.{Definitions, Position}

object Utils {

  implicit class DeclarationDescriptorTranslator(desc: DeclarationDescriptor) {

    def toJsName: String = encodeName(desc.getName.asString())

    def toIdent(implicit pos: Position): Ident =
      Ident(desc.toJsName, Some(desc.getName.asString()))
  }

  implicit class FunctionDescriptorTranslator(desc: FunctionDescriptor) {

    def toMethodIdent(implicit pos: Position): Ident = {
      val owner = desc.getContainingDeclaration match {
        case c: ClassDescriptor => c
        case _ => throw new Error("Not supported, function outside classes...")
      }
      encodeMethodIdent(desc, owner)
    }
  }

  implicit class ClassDescriptorTranslator(desc: ClassDescriptor) {

    def toClassName: String = encodeClassFullName(desc)

    def toClassIdent(implicit pos: Position): Ident =
      encodeClassFullNameIdent(desc)

    def toJsClassType: ClassType = ClassType(desc.toClassName)
  }

  implicit class ValueParameterTranslator(v: ValueParameterDescriptor) {

    def toVarRef(implicit pos: Position): VarRef =
      VarRef(v.toIdent)(v.getType.toJsType)

    def toParamDef(implicit pos: Position): ParamDef =
      ParamDef(v.toIdent, v.getType.toJsType, v.isVar, rest = false)
  }

  implicit class TypeTranslator(kTpe: KotlinType) {

    def toJsType: Type = getType(kTpe.toString)

    def toJsInternal: String = getInternal(kTpe.toJsType)
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
      NoType
  }

  private def getInternal(tpe: Type): String = tpe match {
    case NothingType => Definitions.RuntimeNothingClass
    case NullType => Definitions.RuntimeNullClass
    case ArrayType(elem, _) => "A" + encodeName(elem.toString)
    case other => primitiveCharCode(other)
  }

  private def primitiveCharCode(tpe: Type): String = tpe match {
    case NoType => "V" // TODO: Check if really a good idea...
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
