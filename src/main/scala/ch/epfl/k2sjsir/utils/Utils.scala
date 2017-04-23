package ch.epfl.k2sjsir.utils

import ch.epfl.k2sjsir.utils.NameEncoder._
import org.jetbrains.kotlin.descriptors._
import org.jetbrains.kotlin.resolve.DescriptorUtils._
import org.jetbrains.kotlin.types.{KotlinType, TypeUtils}
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types._
import org.scalajs.core.ir.{Definitions, Position, Types}

import scala.collection.JavaConverters._

object Utils {

  implicit class DeclarationDescriptorTranslator(d: DeclarationDescriptor) {
    private val name = d.getName.asString()

    def toJsName: String = encodeName(name)

    def toJsIdent(implicit pos: Position): Ident = Ident(d.toJsName, Some(name))
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
    private val tpe = d.getReturnType.toJsType

    def toJsParamDef(implicit pos: Position): ParamDef = ParamDef(d.toJsIdent, tpe, mutable = false, rest = false)

    def toJsInternal: String = toInternal(tpe)
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

    def toJsClassType: ClassType = getClassType(t)

    def toJsArrayType: ArrayType = getArrayType(t)

    def toJsInternal: String = toInternal(t.toJsType)
  }

  def getName(tpe: KotlinType): String = {
    val desc = tpe.getConstructor.getDeclarationDescriptor
    if (TypeUtils.isTypeParameter(tpe)) {
      val tps = desc.asInstanceOf[TypeParameterDescriptor].getUpperBounds
      if (tps.isEmpty) "kotlin.Any"
      else getFqName(tps.get(0).getConstructor.getDeclarationDescriptor).asString()
    }
    else getFqName(desc).asString()
  }

  private def getType(tpe: KotlinType, isVararg: Boolean = false): Type = {
    types.getOrElse(getName(tpe), getRefType(tpe).asInstanceOf[Type])
  }

  private def getRefType(tpe: KotlinType): ReferenceType = {
    val name = getName(tpe)
    if (name == "kotlin.Array" || arrayTypes.contains(name)) getArrayType(tpe)
    else if (name.startsWith("kotlin.Function")) getFunctionType(name)
    else ClassType(toInternal(types.getOrElse(name, getClassType(tpe))))
  }

  private def getFunctionType(name: String): ReferenceType = {
    val suffix = name.replace("kotlin.Function", "")
    ClassType(s"F$suffix")
  }

  private def getClassType(tpe: KotlinType): ClassType =
    ClassType(encodeClassName(getName(tpe), ""))

  private def getArrayType(tpe: KotlinType): ArrayType = {
    val name = getName(tpe)
    val args = tpe.getArguments.asScala.map(_.getType)
    if (name == "kotlin.Array") ArrayType(getRefType(args.head))
    else arrayTypes(name)
  }

  private val arrayTypes = Map(
    "kotlin.IntArray" -> IntType,
    "kotlin.BooleanArray" -> BooleanType,
    "kotlin.CharArray" -> IntType,
    "kotlin.ByteArray" -> IntType,
    "kotlin.ShortArray" -> IntType,
    "kotlin.IntArray" -> IntType,
    "kotlin.FloatArray" -> FloatType,
    "kotlin.LongArray" -> LongType,
    "kotlin.DoubleArray" -> DoubleType
  ).mapValues(t => ArrayType(ClassType(toInternal(t))))

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

  private def toInternal(t: Type): String = t match {
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

  val isValueType = Set[Types.Type](IntType, LongType, DoubleType, BooleanType, FloatType)

}
