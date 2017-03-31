package ch.epfl.k2sjsir.utils

import ch.epfl.k2sjsir.utils.NameEncoder._
import org.jetbrains.kotlin.descriptors._
import org.jetbrains.kotlin.descriptors.impl.AbstractTypeParameterDescriptor
import org.jetbrains.kotlin.types.KotlinType
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.{ReferenceType, _}
import org.scalajs.core.ir.{Definitions, Position}

import scala.collection.JavaConverters._

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
    def toJsType: Type = getType(t.getConstructor.getDeclarationDescriptor)
    def toJsInternal: String = getInternal(t.toJsType)
  }

  private def getType(tpe: ClassifierDescriptor): Type = {
    val name = tpe.getName.asString()
    types.getOrElse(name, getRefType(tpe).asInstanceOf[Type])
  }

  private def getRefType(tpe: ClassifierDescriptor): ReferenceType = tpe match {
    case _: AbstractTypeParameterDescriptor => ClassType("O")
    case c: ClassDescriptor =>
      val name = tpe.getName.asString()
      if (name == "Array") {
        val t = c.getTypeConstructor.getParameters.asScala
        ArrayType(getRefType(t.head))
      } else c.toJsClassType
    case x => ClassType(x.toJsName)
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
    "Null" -> NullType,
    "String" -> ClassType("T")
  )

  private def getInternal(t: Type): String = t match {
    case NoType => "V"
    case AnyType => "O"
    case BooleanType => "Z"
    case IntType => "I"
    case LongType => "J"
    case FloatType => "F"
    case DoubleType => "D"
    case StringType | ClassType("T") => "T"
    case ArrayType(elem, _) => "A" + encodeName(elem)
    case ClassType(name) => name
    case NothingType => Definitions.RuntimeNothingClass
    case NullType => Definitions.RuntimeNullClass
    case _ => throw new Error(s"Unknown Scala.js type: $t")
  }

}
