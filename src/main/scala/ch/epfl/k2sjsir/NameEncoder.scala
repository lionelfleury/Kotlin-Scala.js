package ch.epfl.k2sjsir

import ch.epfl.k2sjsir.Utils._
import org.jetbrains.kotlin.descriptors._
import org.jetbrains.kotlin.resolve.descriptorUtil.DescriptorUtilsKt
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types._
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

  //  private def getClassName(d: ClassDescriptor): String = d.getName.asString() match {
  //    case "Any" => "java.lang.Object"
  //    case x => x
  //  }

  val nonValid = Set('<', '-', '?', '>')
  def isInit(s: String): Boolean = s == "<clinit>" || s == "<init>"

  def encodeName(name: String): String = {
    val n = name.filterNot(nonValid)
    mangleJSName(n)
  }

  def encodeClassFullNameIdent(d: ClassDescriptor)(implicit pos: Position): Ident =
    Ident(encodeClassFullName(d))

  def encodeClassFullName(d: ClassDescriptor): String = {
    val suffix = if (d.getKind == ClassKind.OBJECT) "$" else ""
    val name = d.getName.toJsName
    Definitions.encodeClassName(name + suffix)
  }

  def encodeMethodIdent(d: FunctionDescriptor,
                        owner: ClassDescriptor,
                        reflProxy: Boolean = false)(implicit pos: Position): Ident = {
    val (encodedName, paramsString) = encodeMethodNameInternal(d, owner, reflProxy)
    Ident(encodedName + paramsString, Some(d.getName.toJsName + paramsString))
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

    val encodedName = encodeName(name)
    //      if (isInit(name)) trim(name) + InnerSep
    //      else if (isGetter(name)) trim(name).replace("-", "_") + InnerSep
    //      else if (isPrivate(name)) mangleJSName(name) + OuterSep + "p" + privateSuffix(owner)
    //      else mangleJSName(name)

    val paramsString = makeParamsString(d, owner, reflProxy, inRTClass)

    (encodedName, paramsString)
  }

  private def makeParamsString(d: FunctionDescriptor, owner: ClassDescriptor, reflProxy: Boolean, inRTClass: Boolean): String = {
    val tpe = d.getTypeParameters

    val paramTypeNames0 = d.getTypeParameters.map { t =>
      internalName(t.getDefaultType.toJsType)
    }

    //    val hasExplicitThisParameter =
    //      inRTClass || isScalaJSDefinedJSClass(sym.owner)
    //    val paramTypeNames =
    //      if (!hasExplicitThisParameter) paramTypeNames0
    //      else internalName(sym.owner.toTypeConstructor) :: paramTypeNames0

    val paramAndResultTypeNames = paramTypeNames0
    //      if (sym.isClassConstructor)
    //        paramTypeNames
    //      else if (reflProxy)
    //        paramTypeNames :+ ""
    //      else
    //        paramTypeNames :+ internalName(tpe.resultType)

    makeParamsString(paramAndResultTypeNames.toList)
  }

  private def makeParamsString(paramAndResultTypeNames: List[String]) =
    paramAndResultTypeNames.mkString(OuterSep, OuterSep, "")

  private def encodeMemberNameInternal(s: String): String = s.replace("_", "$und")

  private def internalName(tpe: Type): String = tpe match {
    case NoType => "V"
    case NothingType => Definitions.RuntimeNothingClass
    case NullType => Definitions.RuntimeNullClass
    case ClassType(name) => name
    case ArrayType(elem, _) => "A" + encodeName(elem.toString)
    case other => encodeName(other.toString)
  }


  private def mangleJSName(name: String) =
    if (isKeyword(name) || name(0).isDigit || name(0) == '$') "$" + name
    else name

}
