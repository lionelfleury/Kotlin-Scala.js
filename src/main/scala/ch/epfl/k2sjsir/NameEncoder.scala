package ch.epfl.k2sjsir

import org.jetbrains.kotlin.descriptors._
import org.jetbrains.kotlin.resolve.descriptorUtil.DescriptorUtilsKt
import org.jetbrains.kotlin.types.KotlinType
import org.jetbrains.kotlin.types.typeUtil.TypeUtilsKt._
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.{Definitions, Position}

import scala.collection.JavaConversions._
import scala.language.implicitConversions

object NameEncoder {
  /** Outer separator string (between parameter types) */
  val OuterSep = "__"

  /** Inner separator character (replace dots in full names) */
  val InnerSep = "_"

  /** Name given to the local Scala.js environment variable */
  val ScalaJSEnvironmentName = "ScalaJS"

  /** Name given to all exported stuff of a class for DCE */
  val dceExportName = "<exported>"

  private def getClassName(d: ClassDescriptor): String = d.getName.asString() match {
    case "Any" => "java.lang.Object"
    case x => x
  }

  def encodeClassFullNameIdent(d: ClassDescriptor)(implicit pos: Position): Ident =
    Ident(encodeClassFullName(d))

  def encodeClassFullName(d: ClassDescriptor): String = {
    val suffix = if (d.getKind == ClassKind.OBJECT) "$" else ""
    val name = getClassName(d)
    Definitions.encodeClassName(name + suffix)
  }

  def encodeMethodIdent(d: FunctionDescriptor,
                        owner: ClassDescriptor,
                        reflProxy: Boolean = false)(implicit pos: Position): Ident = {
    val (encodedName, paramsString) = encodeMethodNameInternal(d, owner, reflProxy)
    Ident(encodedName + paramsString, Some(d.getName.asString() + paramsString))
  }

  private def trim(s: String) = s.substring(1, s.length - 1)

  def encodeName(name: String): String = {
    val n = if (name.startsWith("<")) trim(name) else name
    mangleJSName(n).replace("-", "$")
  }

  private def encodeMethodNameInternal(d: FunctionDescriptor,
                                       owner: ClassDescriptor,
                                       reflProxy: Boolean = false,
                                       inRTClass: Boolean = false): (String, String) = {
    //        require(sym.isMethod, "encodeMethodSym called with non-method symbol: " + sym)

    val name = encodeMemberNameInternal(d.getName.asString())

    def privateSuffix(owner: ClassDescriptor): String =
      if (owner.getKind == ClassKind.INTERFACE && !owner.isImpl) encodeClassFullName(owner)
      else DescriptorUtilsKt.getAllSuperclassesWithoutAny(owner).count(_.getKind != ClassKind.INTERFACE).toString
    //                    if (owner.isTraitOrInterface && !owner.isImplClass) encodeClassFullName(owner)
    //                    else owner.ancestors.count(!_.isTraitOrInterface).toString

    def isGetter(s: String) = s.startsWith("<get-")
    def isInit(s: String) = s == "<clinit>" || s == "<init>"
    def isPrivate(s: String) = false

    val encodedName = encodeName(name)
    //      if (isInit(name)) trim(name) + InnerSep
    //      else if (isGetter(name)) trim(name).replace("-", "_") + InnerSep
    //      else if (isPrivate(name)) mangleJSName(name) + OuterSep + "p" + privateSuffix(owner)
    //      else mangleJSName(name)

    val paramsString = makeParamsString(d, owner, reflProxy, inRTClass)

    (encodedName, paramsString)
  }

  private def encodeMemberNameInternal(s: String): String = s.replace("_", "$und")

  private def mangleJSName(name: String): String = {
    val cond = isKeyword(name) || name.head.isDigit || name.head == '$'
    if (cond) "$" + name else name
  }

  /** Computes the internal name for a type. */
  private def internalName[T <: KotlinType](tpe: T): String = {
    if (isUnit(tpe)) "V"
    //    else if (isPrimitiveNumberType(tpe)) tpe.toString
    else if (isNothing(tpe)) Definitions.RuntimeNothingClass
    else if (isNullableAny(tpe)) Definitions.RuntimeNullClass
    else tpe.toString
  }

  private def makeParamsString(d: FunctionDescriptor,
                               owner: ClassDescriptor,
                               reflProxy: Boolean,
                               inRTClass: Boolean): String = {

    val tpe = d.getReturnType
    val paramTypeNames0 = for (p <- d.getTypeParameters) yield internalName(p.getDefaultType)

    //    val hasExplicitThisParameter = inRTClass //|| isScalaJSDefinedJSClass(owner)
    //    val paramTypeNames: List < String > =
    //    if (! hasExplicitThisParameter) paramTypeNames0
    //    else listOf (internalName (owner.defaultType), * paramTypeNames0.toTypedArray () )
    //
    //    val paramAndResultTypeNames: List < String > =
    //    if (sym.isClInit () ) paramTypeNames
    //    else if (reflProxy) listOf (* paramTypeNames.toTypedArray (), "")
    //    else listOf (* paramTypeNames.toTypedArray (), internalName (tpe) )

    val paramAndResultTypeNames = Nil

    makeParamsString(paramAndResultTypeNames)
  }

  private def makeParamsString(paramAndResultTypeNames: Seq[String]) =
    paramAndResultTypeNames.mkString(OuterSep, OuterSep, "")

}
