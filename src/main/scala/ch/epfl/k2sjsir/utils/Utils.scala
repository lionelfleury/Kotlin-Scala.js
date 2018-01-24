package ch.epfl.k2sjsir.utils

import ch.epfl.k2sjsir.utils.NameEncoder._
import org.jetbrains.kotlin.descriptors.impl.AnonymousFunctionDescriptor
import org.jetbrains.kotlin.descriptors.{ClassKind => KtClassKind, _}
import org.jetbrains.kotlin.fileClasses.JvmFileClassUtil
import org.jetbrains.kotlin.psi.{KtFile, KtProperty}
import org.jetbrains.kotlin.resolve.DescriptorUtils
import org.jetbrains.kotlin.resolve.DescriptorUtils._
import org.jetbrains.kotlin.resolve.`lazy`.descriptors.LazyPackageDescriptor
import org.jetbrains.kotlin.types.{KotlinType, TypeUtils}
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types._
import org.scalajs.core.ir.{ClassKind, Definitions, Position, Types}

import scala.collection.JavaConverters._
import scala.collection.immutable.List

object Utils {

  implicit class DeclarationDescriptorTranslator(d: DeclarationDescriptor) {
    private val name = d.getName.asString()
    private val numParents = getNbOfParentDeclarations

    def toJsClosureName: String = "closureargs$" + encodeName(name)

    def toJsName: String = encodeName(name + numParents.map(n => "$" + n).getOrElse(""))

    def toJsIdent(implicit pos: Position): Ident = Ident(d.toJsName, Some(name))

    def toJsClosureIdent(implicit pos: Position): Ident = Ident(d.toJsClosureName, Some(name))

    private def getNbOfParentDeclarations: Option[Int] = {
      val count = d.getContainingDeclaration match {
        case c: ClassDescriptor => DescriptorUtils.getSuperclassDescriptors(c).asScala.count(!_.isInterface)
        case _ => 0
      }

      if (count == 0 || name == "<this>") None else Some(count)
    }
  }

  implicit class CallableDescriptorTranslator(d: CallableDescriptor) {
    def toJsMethodIdent(implicit pos: Position): Ident = encodeMethodIdent(d)

    def toJsMethodDeclIdent(implicit pos: Position): Ident = encodeMethodIdent(d, kotlinNaming = true)

    def toJsBridgeIdent(implicit  pos: Position): Ident = {
      val clsName = d match {
        case fd: FunctionDescriptor => Option(fd.getContainingDeclaration.asInstanceOf[ClassDescriptor].toJsClassName)
      }

      val toReplace = clsName.fold("")(x => x + "__")

      val tmpIdt = d.toJsMethodIdent

      Ident(tmpIdt.name.replaceFirst(toReplace, ""), tmpIdt.originalName)
    }

    def isTopLevel: Boolean = d.getContainingDeclaration match {
      case _: LazyPackageDescriptor => true
      case _ => false
    }

    def hasExternalParent: Boolean = {
      val cls = DescriptorUtils.getContainingClass(d)
      cls != null && cls.isExternal
    }

    def isAnonymousFunction: Boolean = d.isInstanceOf[AnonymousFunctionDescriptor]

    def getNameEvenIfAnon: String = d.getName.toString.replace("<no name provided>", "anonymous")
  }

  implicit class ClassDescriptorTranslator(d: ClassDescriptor) {
    def toJsClassName: String = encodeClassFullName(d)

    def toJsDefaultImplName: String = encodeClassFullName(d, kotlinNaming = true)

    def toJsEnumCompanionName: String = encodeClassFullName(d, kotlinNaming = true)

    def toJsClassIdent(implicit pos: Position): Ident = encodeClassFullNameIdent(d)

    def toJsDefaultImplIdent(implicit pos: Position): Ident = encodeClassFullNameIdent(d, kotlinNaming = true)

    def toJsEnumCompanionIdent(implicit pos: Position): Ident = encodeClassFullNameIdent(d, kotlinNaming = true)

    def toJsClassType: ClassType = ClassType(d.toJsClassName)

    def toJsDefaultImplType(implicit pos: Position): ClassType = ClassType(d.toJsDefaultImplName)

    def toJsEnumCompanionType(implicit pos: Position): ClassType = ClassType(d.toJsEnumCompanionName)

    def toJsClassKind: ClassKind = getClassKind(d.getKind)

    def isInterface: Boolean = DescriptorUtils.isInterface(d)

    def isEnumClass: Boolean = DescriptorUtils.isEnumClass(d)

    def isEnumEntry: Boolean = DescriptorUtils.isEnumEntry(d)

    def isJLEnum: Boolean = d.toJsClassType == ClassType("jl_Enum")

    def isAnonymous: Boolean = DescriptorUtils.getFqName(d).toString.contains("<no name provided>")

    def getNameEvenIfAnon: String = DescriptorUtils.getFqName(d).toString.replace("<no name provided>", "NoNameProvided")
  }

  implicit class ParameterTranslator(d: ParameterDescriptor) {
    private val tpe = d.getReturnType.toJsType

    def toJsClosureParamDef(implicit pos: Position): ParamDef = _toJsParamDef(Some(AnyType), Some(d.toJsClosureIdent))

    def toJsAnyParamDef(implicit pos: Position): ParamDef = _toJsParamDef(Some(AnyType))

    def toJsParamDef(implicit pos: Position): ParamDef = _toJsParamDef()

    def toJsInternal: String = toInternal(tpe)

    private def _toJsParamDef(paramTpe: Option[Type] = None, ident: Option[Ident] = None)(implicit pos: Position) = {
      ParamDef(ident.getOrElse(d.toJsIdent), paramTpe.getOrElse(tpe), mutable = false, rest = false)
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

    def toJsTypeRef: TypeRef = getTypeRef(t)

    def toJsClassType: ClassType = getClassType(t)

    def toJsArrayType: ArrayType = getArrayType(t)

    def toJsInternal: String = toInternal(t.toJsType)
  }

  implicit class KtPropertyHelper(p: KtProperty) {
    def hasGetterImpl: Boolean = p.getGetter != null

    def hasSetterImpl: Boolean = p.isVar && p.getSetter != null

    def hasDefinedAccessors: Boolean = hasGetterImpl || hasSetterImpl
  }

  implicit class PropertyAccessor(d: PropertyDescriptor)(implicit pos: Position) {
    def getterIdent(): Ident = NameEncoder.encodeMethodIdent(d.getGetter)
    def setterIdent(): Ident = NameEncoder.encodeMethodIdent(d.getSetter)
  }

  implicit class TopLevelHelper(f: KtFile) {
    private val clsName =  JvmFileClassUtil.getFileClassInfoNoResolve(f).getFileClassFqName.asString()

    def toJsClassType: ClassType = ClassType(encodeClassName(clsName, ""))

    def getFileClassName = clsName
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

  def getTypeForIs(tpe: String): TypeRef = tpe match {
    case "I" => ClassRef(Definitions.BoxedIntegerClass)
    case "D" => ClassRef(Definitions.BoxedDoubleClass)
    case "F" => ClassRef(Definitions.BoxedFloatClass)
    case "Z" => ClassRef(Definitions.BoxedBooleanClass)
    case "J" => ClassRef(Definitions.BoxedLongClass)
    case other => ClassRef(other)
  }

  def getFreshName(prefix: String = "x$"): String = {
    prefix + java.util.UUID.randomUUID().toString.replaceAllLiterally("-", "")
  }

  def ensureBoxed(t: Tree): Tree = {
    if (t.tpe == NoType)
      Block(t, Undefined()(t.pos))(t.pos)
    else
      t
  }

  def cast(t: Tree, rtpe: KotlinType)(implicit pos: Position): Tree = {
    // TODO: Add a function to determine if it's a JS type
    if (rtpe.toJsType == NoType || isLambdaType(rtpe) || t.tpe == rtpe.toJsType) {
      t
    } else if (Utils.isPrimitiveType(rtpe.toJsType)) {
      Unbox(t, rtpe.toJsInternal.charAt(0))
    } else {
      AsInstanceOf(t, rtpe.toJsTypeRef)
    }
  }

  private def getType(tpe: KotlinType, isVararg: Boolean = false): Type = {
    if (isLambdaType(tpe)) AnyType
    else {
      val ret = getTypeRef(tpe) match {
        case ClassRef(cls) => ClassType(cls)
        case typeRef: ArrayTypeRef => ArrayType(typeRef)
      }

      val nullable = TypeUtils.isNullableType(tpe)
      val suffix = if (nullable) "?" else ""

      types.getOrElse(getName(tpe) + suffix, ret)
    }
  }

  def nullableToNonNullable(tpe: KotlinType): KotlinType = {
    tpe.getConstructor.getDeclarationDescriptor.getDefaultType
  }

  def adaptPrimitive(value: Tree, to: Type)(
    implicit pos: Position): Tree = {
    genConversion(value.tpe, to, value)
  }

  /* This method corresponds to the method of the same name in
   * BCodeBodyBuilder of the JVM back-end. It ends up calling the method
   * BCodeIdiomatic.emitT2T, whose logic we replicate here.
   */
  private def genConversion(from: Type, to: Type, value: Tree)(
    implicit pos: Position): Tree = {
    import UnaryOp._

    if (from == to || from == NothingType) {
      value
    } else if (from == BooleanType || to == BooleanType) {
      throw new AssertionError(s"Invalid genConversion from $from to $to")
    } else {
      def intValue = (from: @unchecked) match {
        case IntType    => value
        case CharType   => UnaryOp(CharToInt, value)
        case ByteType   => UnaryOp(ByteToInt, value)
        case ShortType  => UnaryOp(ShortToInt, value)
        case LongType   => UnaryOp(LongToInt, value)
        case FloatType  => UnaryOp(DoubleToInt, UnaryOp(FloatToDouble, value))
        case DoubleType => UnaryOp(DoubleToInt, value)
      }

      def doubleValue = from match {
        case DoubleType => value
        case FloatType  => UnaryOp(FloatToDouble, value)
        case LongType   => UnaryOp(LongToDouble, value)
        case _          => UnaryOp(IntToDouble, intValue)
      }

      (to: @unchecked) match {
        case CharType =>
          UnaryOp(IntToChar, intValue)
        case ByteType =>
          UnaryOp(IntToByte, intValue)
        case ShortType =>
          UnaryOp(IntToShort, intValue)
        case IntType =>
          intValue
        case LongType =>
          from match {
            case FloatType | DoubleType =>
              UnaryOp(DoubleToLong, doubleValue)
            case _ =>
              UnaryOp(IntToLong, intValue)
          }
        case FloatType =>
          UnaryOp(UnaryOp.DoubleToFloat, doubleValue)
        case DoubleType =>
          doubleValue
      }
    }
  }

  def isPrimitiveType(value: Type): Boolean = {
    value match {
      case BooleanType | CharType | ByteType |
           ShortType | IntType | LongType | FloatType |
           DoubleType =>
        true
      case _ => false
    }
  }

  def genThisFromContext(tpe: Type, clsDesc: ClassDescriptor)(implicit pos: Position): Tree = {
    if (clsDesc != null && clsDesc.toJsClassKind == ClassKind.Interface)
      VarRef(Ident("$this"))(clsDesc.toJsClassType)
    else
      This()(tpe)
  }

  def genThisFromContext(tpe: Type, cmd: CallableMemberDescriptor = null)(implicit pos: Position): Tree = {
    if (cmd != null) {

      val dr = cmd.getDispatchReceiverParameter

      if (dr != null) {
        val clsDesc = DescriptorUtils.getClassDescriptorForType(dr.getType)

        return genThisFromContext(tpe, clsDesc)
      }

      cmd match {
        case a: AnonymousFunctionDescriptor =>
          val clsDesc = DescriptorUtils.getContainingClass(a)
          return VarRef(Ident("$this"))(clsDesc.toJsClassType)
        case f: FunctionDescriptor if DescriptorUtils.isExtension(f) =>
          return VarRef(Ident("$this"))(tpe)
        case _ => /* Skip */
      }
    }

    This()(tpe)
  }

  def genStructuralEq(lhs: Tree, rhs: Tree)(implicit pos: Position): Tree = {

    val lhsName = Ident(Utils.getFreshName())
    val rhsName = Ident(Utils.getFreshName())

    val lhsDef = VarDef(lhsName, lhs.tpe, false, lhs)
    val rhsDef = VarDef(rhsName, rhs.tpe, false, rhs)

    val lhsRef = VarRef(lhsName)(lhs.tpe)
    val rhsRef = VarRef(rhsName)(rhs.tpe)

    val equalsIdent = Ident("equals__O__Z", Some("equals"))

    val finalIf = If(
      genNullCond(lhsRef),
      genNullCond(rhsRef),
      Apply(lhsRef, equalsIdent, List(rhsRef))(BooleanType)
    )(BooleanType)

    val block = Block(List(
      lhsDef,
      rhsDef,
      finalIf
    ))

    block
  }

  def genNullCond(t: Tree)(implicit pos: Position): Tree = {
    BinaryOp(
      BinaryOp.===,
      t,
      Null()
    )
  }

  def genNotNullCond(t: Tree)(implicit pos: Position): Tree = {
    BinaryOp(
      BinaryOp.!==,
      t,
      Null()
    )
  }

  private def isLambdaType(tpe: KotlinType): Boolean = {
    tpe.toString.matches("Function[0-9]+.*")
  }

  private def getTypeRef(tpe: KotlinType): TypeRef = {
    val suffix = if (TypeUtils.isNullableType(tpe)) "?" else ""
    val name = getName(tpe)
    if (name == "kotlin.Array" || arrayTypes.contains(name)) getArrayTypeRef(tpe)
    else if (name.startsWith("kotlin.Function")) getFunctionType(name)
    else ClassRef(toInternal(types.getOrElse(name + suffix, getClassType(tpe))))
  }

  private def getTypeRef(tpe: Type): TypeRef = tpe match {
    case ClassType(name) => ClassRef(name)
    case ArrayType(arrayTypeRef) => arrayTypeRef
    case t => ClassRef(toInternal(t))
  }

  private def getFunctionType(name: String): TypeRef = {
    // FIXME: Keep kotlin.Function when stdlib is working
    val suffix = name.replace("kotlin.Function", "")
    ClassRef(s"sjs_js_Function$suffix")
  }

  private def getClassType(tpe: KotlinType): ClassType = {
    val suffix = TypeUtils.getClassDescriptor(tpe) match {
      case cd: ClassDescriptor if isCompanionObject(cd) || isObject(cd) => "$"
      case _ => ""
    }

    ClassType(encodeClassName(getName(tpe), suffix))
  }

  private def getClassKind(kind: KtClassKind): ClassKind = {
    classKinds(kind)
  }

  private def getArrayTypeRef(tpe: KotlinType): ArrayTypeRef = {
    val name = getName(tpe)
    val args = tpe.getArguments.asScala.map(_.getType)
    if (name == "kotlin.Array") ArrayTypeRef.of(getTypeRef(args.head))
    else arrayTypes(name)
  }

  private def getArrayType(tpe: KotlinType): ArrayType = {
    ArrayType(getArrayTypeRef(tpe))
  }

  private val arrayTypes = Map(
    "kotlin.IntArray" -> ClassRef("I"),
    "kotlin.BooleanArray" -> ClassRef("Z"),
    "kotlin.CharArray" -> ClassRef("C"),
    "kotlin.ByteArray" -> ClassRef("B"),
    "kotlin.ShortArray" -> ClassRef("S"),
    "kotlin.FloatArray" -> ClassRef("F"),
    "kotlin.LongArray" -> ClassRef("J"),
    "kotlin.DoubleArray" -> ClassRef("D")
  ).mapValues(t => ArrayTypeRef.of(t))

  private val types = Map(
    "kotlin.Any" -> AnyType,
    "kotlin.Unit" -> NoType,
    "kotlin.Nothing" -> NothingType,
    "kotlin.Boolean" -> BooleanType,
    "kotlin.Char" -> CharType,
    "kotlin.Byte" -> ByteType,
    "kotlin.Short" -> ShortType,
    "kotlin.Int" -> IntType,
    "kotlin.Float" -> FloatType,
    "kotlin.Long" -> LongType,
    "kotlin.Double" -> DoubleType,
    "kotlin.Null" -> NullType,
    "kotlin.String" -> ClassType(Definitions.StringClass),
    "kotlin.Throwable" -> AnyType,
    "kotlin.Comparable" -> AnyType,

    "kotlin.Any?" -> AnyType,
    "kotlin.Boolean?" -> ClassType(Definitions.BoxedBooleanClass),
    "kotlin.Char?" -> ClassType(Definitions.BoxedCharacterClass),
    "kotlin.Byte?" -> ClassType(Definitions.BoxedByteClass),
    "kotlin.Short?" -> ClassType(Definitions.BoxedShortClass),
    "kotlin.Int?" -> ClassType(Definitions.BoxedIntegerClass),
    "kotlin.Float?" -> ClassType(Definitions.BoxedFloatClass),
    "kotlin.Long?" -> ClassType(Definitions.BoxedLongClass),
    "kotlin.Double?" -> ClassType(Definitions.BoxedDoubleClass),
    "kotlin.String?" -> ClassType(Definitions.StringClass),
    "kotlin.Throwable?" -> AnyType,
    "kotlin.Comparable?" -> AnyType

  )

  private val boxedToPrimitive = Map(
    ClassType(Definitions.BoxedBooleanClass) -> BooleanType,
    ClassType(Definitions.BoxedCharacterClass) -> CharType,
    ClassType(Definitions.BoxedByteClass) -> ByteType,
    ClassType(Definitions.BoxedShortClass) -> ShortType,
    ClassType(Definitions.BoxedIntegerClass) -> IntType,
    ClassType(Definitions.BoxedFloatClass) -> FloatType,
    ClassType(Definitions.BoxedLongClass) -> LongType,
    ClassType(Definitions.BoxedDoubleClass) -> DoubleType
  )

  private val classKinds = Map(
    KtClassKind.CLASS -> ClassKind.Class,
    KtClassKind.INTERFACE -> ClassKind.Interface,
    KtClassKind.OBJECT -> ClassKind.ModuleClass,
    KtClassKind.ENUM_CLASS -> ClassKind.Class,
    KtClassKind.ENUM_ENTRY -> ClassKind.Class
  )

  private[utils] def toInternal(t: Type): String = t match {
    case NoType => "V"
    case AnyType => Definitions.ObjectClass
    case BooleanType => "Z"
    case IntType => "I"
    case LongType => "J"
    case FloatType => "F"
    case DoubleType => "D"
    case StringType => "T"
    case ByteType => "B"
    case ShortType => "S"
    case CharType => "C"
    case ArrayType(ArrayTypeRef(elem, dims)) => "A"*dims + encodeName(elem)
    // FIXME: Remove this after kotlin-stdlib is compiled correctly
    case ClassType(name) if name.matches("kotlin.Function*") => name.replace("kotlin.Function", "sjs_js_Function")
    case ClassType(name) => name.replace("<no name provided>", "NoNameProvided")
    case NothingType => Definitions.RuntimeNothingClass
    case NullType => Definitions.RuntimeNullClass
    case _ => throw new Error(s"Unknown Scala.js type: $t")
  }

  private val isValueType = Set[Types.Type](IntType, LongType, DoubleType, BooleanType, FloatType)

}
