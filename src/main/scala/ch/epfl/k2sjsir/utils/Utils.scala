package ch.epfl.k2sjsir.utils

import ch.epfl.k2sjsir.utils.NameEncoder._
import org.jetbrains.kotlin.descriptors._
import org.jetbrains.kotlin.resolve.DescriptorUtils.getClassDescriptorForType
import org.jetbrains.kotlin.types.KotlinType
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types._
import org.scalajs.core.ir.{Definitions, Position}

object Utils {

  implicit class DeclarationDescriptorTranslator(d: DeclarationDescriptor) {
    def toJsName: String = encodeName(d.getName.asString())
    def toJsIdent(implicit pos: Position): Ident = Ident(d.toJsName, Some(d.getName.asString()))
  }

  implicit class CallableDescriptorTranslator(d: CallableDescriptor) {
    def toJsMethodIdent(implicit pos: Position): Ident = encodeMethodIdent(d)
  }

  implicit class ClassDescriptorTranslator(d: ClassDescriptor) {
    def toJsClassName: String = encodeClassFullName(d)
    def toJsClassIdent(implicit pos: Position): Ident = encodeClassFullNameIdent(d)
    def toJsClassType: ClassType = ClassType(d.toJsClassName)
  }

  implicit class ValueParameterTranslator(d: ValueParameterDescriptor) {
    def toJsVarRef(implicit pos: Position): VarRef = VarRef(d.toJsIdent)(d.getType.toJsType)
    def toJsParamDef(implicit pos: Position): ParamDef = ParamDef(d.toJsIdent, d.getType.toJsType, d.isVar, rest = false)
  }

  implicit class KotlinTypeTranslator(t: KotlinType) {
    def toJsType: Type = types.getOrElse(t.toString, getClassDescriptorForType(t).toJsClassType)
    def toJsInternal: String = getInternal(t.toJsType)
  }

  private val types = Map(
    "Any" -> AnyType,
    "Unit" -> NoType,
    "Nothing" -> NothingType,
    "Boolean" -> BooleanType,
    "Char" -> IntType,
    "Byte" -> IntType,
    "Short" -> IntType,
    "Int" -> IntType,
    "Float" -> FloatType,
    "Long" -> LongType,
    "Double" -> DoubleType,
    "Null" -> NullType
  )

  private def getInternal(t: Type): String = t match {
    case NoType => "V"
    case AnyType => "O"
    case BooleanType => "Z"
    case IntType => "I"
    case LongType => "J"
    case FloatType => "F"
    case DoubleType => "D"
    case ArrayType(elem, _) => "A" + encodeName(elem.toString)
    case ClassType(name) => Definitions.encodeClassName(name)
    case NothingType => Definitions.RuntimeNothingClass
    case NullType => Definitions.RuntimeNullClass
    case _ => throw new Error(s"Unknown Scala.js type: $t")
  }

}
