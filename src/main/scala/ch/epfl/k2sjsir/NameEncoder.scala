package ch.epfl.k2sjsir

import ch.epfl.k2sjsir.Utils._
import org.jetbrains.kotlin.descriptors.ClassKind.{INTERFACE, OBJECT}
import org.jetbrains.kotlin.descriptors.{ClassDescriptor, FunctionDescriptor}
import org.jetbrains.kotlin.resolve.descriptorUtil.DescriptorUtilsKt.getAllSuperclassesWithoutAny
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.{Definitions, Position}

import scala.collection.JavaConversions._
import scala.language.implicitConversions

object NameEncoder {
  /** Outer separator string (between parameter types) */
  private val OuterSep = "__"

  /** Inner separator character (replace dots in full names) */
  private val InnerSep = "_"

  /** Name given to the local Scala.js environment variable */
  private val ScalaJSEnvironmentName = "ScalaJS"

  /** Name given to all exported stuff of a class for DCE */
  private val dceExportName = "<exported>"

  private val nonValid = Set('<', '-', '?', '>')
  private def isInit(s: String): Boolean = s == "<clinit>" || s == "<init>"

  def encodeName(name: String): String = {
    val n = name.filterNot(nonValid)
    if (isKeyword(n) || n(0).isDigit || n(0) == '$') "$" + n
    else n
  }

  def encodeClassFullNameIdent(d: ClassDescriptor)(implicit pos: Position): Ident =
    Ident(encodeClassFullName(d))

  def encodeClassFullName(d: ClassDescriptor): String = {
    val suffix = if (d.getKind == OBJECT) "$" else ""
    val name = d.toJsName
    val n = if (name == "Any") "java.lang.Object" else name
    Definitions.encodeClassName(n + suffix)
  }

  def encodeMethodIdent(d: FunctionDescriptor,
                        owner: ClassDescriptor,
                        reflProxy: Boolean = false)(implicit pos: Position): Ident = {
    val (encodedName, paramsString) = encodeMethodNameInternal(d, reflProxy)
    Ident(encodedName + paramsString, Some(d.toJsName + paramsString))
  }

  def encodeMethodName(d: FunctionDescriptor, reflProxy: Boolean = false): String = {
    val (encodedName, paramsString) = encodeMethodNameInternal(d, reflProxy)
    encodedName + paramsString
  }

  private def encodeMethodNameInternal(d: FunctionDescriptor,
                                       reflProxy: Boolean = false,
                                       inRTClass: Boolean = false): (String, String) = {

    val name = encodeMemberNameInternal(d.getName.asString())

    def privateSuffix(owner: ClassDescriptor): String =
      if (owner.getKind == INTERFACE && !owner.isImpl) encodeClassFullName(owner)
      else getAllSuperclassesWithoutAny(owner).count(_.getKind != INTERFACE).toString

    val owner = d.getContainingDeclaration match {
      case c: ClassDescriptor => c
      case _ => throw new Error("Only function in classes are supported")
    }

    def isPrivate = false

    val encodedName =
      if (isPrivate) encodeName(name) + OuterSep + "p" + privateSuffix(owner)
      else encodeName(name)

    val paramsString = makeParamsString(d, reflProxy, inRTClass)

    (encodedName, paramsString)
  }

  private def makeParamsString(d: FunctionDescriptor, reflProxy: Boolean, inRTClass: Boolean): String = {
    val tpes = d.getValueParameters
    val paramTypeNames0 = tpes.map(_.getType.toJsInternal)

    //    val hasExplicitThisParameter =
    //      inRTClass || isScalaJSDefinedJSClass(sym.owner)
    //        val paramTypeNames =
    //          if (!hasExplicitThisParameter) paramTypeNames0
    //          else internalName(sym.owner.toTypeConstructor) :: paramTypeNames0
    val paramAndResultTypeNames =
    if (isInit(d.getName.toString)) paramTypeNames0
    else paramTypeNames0 :+ d.getReturnType.toJsInternal

    paramAndResultTypeNames.mkString(OuterSep, OuterSep, "")
  }

  private def encodeMemberNameInternal(s: String): String = s.replace("_", "$und")

}
