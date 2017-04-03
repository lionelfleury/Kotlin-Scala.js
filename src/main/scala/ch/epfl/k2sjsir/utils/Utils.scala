package ch.epfl.k2sjsir.utils

import ch.epfl.k2sjsir.utils.NameEncoder._
import org.jetbrains.kotlin.descriptors._
import org.jetbrains.kotlin.resolve.DescriptorUtils._
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

  implicit class ParameterTranslator(d: ParameterDescriptor) {
    def toJsParamDef(implicit pos: Position): ParamDef = d match {
      case dd: ValueParameterDescriptor if isVarArg(d) =>
        val tpe = dd.getVarargElementType.toJsRefType
        ParamDef(d.toJsIdent, ArrayType(tpe), mutable = false, rest = false)
      case _ =>
        ParamDef(d.toJsIdent, d.getReturnType.toJsType, mutable = false, rest = false)
    }
  }

  def isVarArg(d: ParameterDescriptor): Boolean = d match {
    case dd: ValueParameterDescriptor if dd.getVarargElementType != null => true
    case _ => false
  }

  implicit class VariableTranslator(d: VariableDescriptor) {
    def toJsVarRef(implicit pos: Position): VarRef = VarRef(d.toJsIdent)(d.getReturnType.toJsType)
  }

  implicit class KotlinTypeTranslator(t: KotlinType) {
    def toJsType: Type = getType(t)
    def toJsRefType: ReferenceType = getRefType(t)
    def toJsInternal: String = getInternal(t.toJsType)
  }

  private def getType(tpe: KotlinType): Type = {
    val name = getFqName(tpe.getConstructor.getDeclarationDescriptor).asString()
    types.getOrElse(name, getRefType(tpe).asInstanceOf[Type])
  }

  private def getRefType(tpe: KotlinType): ReferenceType = {
    val name = getFqName(tpe.getConstructor.getDeclarationDescriptor).asString()
    ClassType(getInternal(types.getOrElse(name, ClassType(encodeClassName(name, "")))))
  }

  private val types = Map(
    "kotlin.Any" -> AnyType,
    "kotlin.Unit" -> NoType,
    "kotlin.Nothing" -> NothingType,
    "kotlin.Boolean" -> BooleanType,
    "kotlin.Char" -> IntType,
    "kotlin.Byte" -> IntType,
    "kotlin.Short" -> IntType,
    "kotlin.Int" -> IntType,
    "kotlin.Float" -> FloatType,
    "kotlin.Long" -> LongType,
    "kotlin.Double" -> DoubleType,
    "kotlin.Null" -> NullType,
    "kotlin.String" -> ClassType("T"),
    "kotlin.Throwable" -> AnyType
  )

  private def getInternal(t: Type): String = t match {
    case NoType => "V"
    case AnyType => "O"
    case BooleanType => "Z"
    case IntType => "I"
    case LongType => "J"
    case FloatType => "F"
    case DoubleType => "D"
    case StringType => "T"
    case ArrayType(elem, _) => "A" + encodeName(elem)
    case ClassType(name) => name
    case NothingType => Definitions.RuntimeNothingClass
    case NullType => Definitions.RuntimeNullClass
    case _ => throw new Error(s"Unknown Scala.js type: $t")
  }

}
