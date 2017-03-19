package ch.epfl.k2sjsir

import ch.epfl.k2sjsir.Utils._
import org.jetbrains.kotlin.descriptors.ClassKind.{INTERFACE, OBJECT}
import org.jetbrains.kotlin.descriptors.{CallableDescriptor, ClassDescriptor}
import org.jetbrains.kotlin.resolve.descriptorUtil.DescriptorUtilsKt.getAllSuperclassesWithoutAny
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.{Definitions, Position}

import scala.collection.JavaConversions._
import scala.language.implicitConversions

object NameEncoder {
  /** Outer separator string (between parameter types) */
  private val OuterSep = "__"

  /** Inner separator character (replace dots in full names) */
  //  private val InnerSep = "_"

  /** Name given to the local Scala.js environment variable */
  //  private val ScalaJSEnvironmentName = "ScalaJS"

  /** Name given to all exported stuff of a class for DCE */
  //  private val dceExportName = "<exported>"

  private val nonValid = Set('<', '-', '?', '>')
  private def isInit(s: String): Boolean = s == "<clinit>" || s == "<init>"

  def encodeName(name: String): String = {
    val n = name.filterNot(nonValid)
    if (isKeyword(n) || n(0).isDigit || n(0) == '$') "$" + n else n
  }

  def encodeClassFullNameIdent(d: ClassDescriptor)(implicit pos: Position): Ident =
    Ident(encodeClassFullName(d))

  def encodeClassFullName(d: ClassDescriptor): String = {
    val suffix = if (d.getKind == OBJECT) "$" else ""
    val name = d.toJsName
    val n = if (name == "Any") "java.lang.Object" else name
    Definitions.encodeClassName(n + suffix)
  }

  def encodeMethodIdent(d: CallableDescriptor, reflProxy: Boolean = false)(implicit pos: Position): Ident =
    Ident(encodeMethodName(d, reflProxy), Some(d.getName.asString()))

  def encodeMethodName(d: CallableDescriptor, reflProxy: Boolean = false): String =
    encodeMethodNameInternal(d, reflProxy).mkString


  private def encodeMethodNameInternal(d: CallableDescriptor, reflProxy: Boolean = false, inRTClass: Boolean = false): Seq[String] = {
    val name = encodeMemberNameInternal(d.getName.asString())
    def privateSuffix(c: ClassDescriptor) =
      if (c.getKind == INTERFACE && !c.isImpl) encodeClassFullName(c)
      else getAllSuperclassesWithoutAny(c).count(_.getKind != INTERFACE).toString
    //TODO: Only function in classes supported...
    val owner = d.getContainingDeclaration match {
      case c: ClassDescriptor => c
      case x => throw new Error(s"Only Classes are supported for now: $x")
    }
    def isPrivate = false
    val encodedName = if (isPrivate) encodeName(name) + OuterSep + "p" + privateSuffix(owner) else encodeName(name)
    val paramsString = makeParamsString(d, reflProxy, inRTClass)
    Seq(encodedName, paramsString)
  }

  private def makeParamsString(d: CallableDescriptor, reflProxy: Boolean, inRTClass: Boolean): String = {
    val tpes = d.getValueParameters
    val paramTypeNames0 = tpes.map(_.getType.toJsInternal)
    //    val hasExplicitThisParameter =
    //      inRTClass || isScalaJSDefinedJSClass(sym.owner)
    //        val paramTypeNames =
    //          if (!hasExplicitThisParameter) paramTypeNames0
    //          else internalName(sym.owner.toTypeConstructor) :: paramTypeNames0
    val paramAndResultTypeNames = if (isInit(d.getName.asString())) paramTypeNames0 else paramTypeNames0 :+ d.getReturnType.toJsInternal
    paramAndResultTypeNames.mkString(OuterSep, OuterSep, "")
  }

  private def encodeMemberNameInternal(s: String): String = s.replace("_", "$und")

}
