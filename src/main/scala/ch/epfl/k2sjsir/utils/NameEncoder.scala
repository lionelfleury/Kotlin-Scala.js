package ch.epfl.k2sjsir.utils

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.ClassKind.INTERFACE
import org.jetbrains.kotlin.descriptors.{CallableDescriptor, ClassDescriptor, TypeAliasDescriptor, Visibilities}
import org.jetbrains.kotlin.load.java.`lazy`.descriptors.LazyJavaPackageFragment
import org.jetbrains.kotlin.resolve.DescriptorUtils._
import org.jetbrains.kotlin.resolve.`lazy`.descriptors.LazyPackageDescriptor
import org.jetbrains.kotlin.resolve.descriptorUtil.DescriptorUtilsKt.getAllSuperclassesWithoutAny
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.{Definitions, Position}

import scala.collection.JavaConverters._

object NameEncoder {
  /** Outer separator string (between parameter types) */
  private val OuterSep = "__"

  /** Inner separator character (replace dots in full names) */
  private val InnerSep = "_"

  /** Name given to the local Scala.js environment variable */
  //  private val ScalaJSEnvironmentName = "ScalaJS"

  /** Name given to all exported stuff of a class for DCE */
  //  private val dceExportName = "<exported>"

  private val nonValid = Seq("<get-", "<set-", "<", "-", "?", ">")
  private def isInit(s: String): Boolean = s == "<clinit>" || s == "<init>"

  private[utils] def encodeName(name: String): String = {
    val n = nonValid.foldLeft(name)((n, r) => n.replace(r, ""))
    if (isKeyword(n) || n(0).isDigit || n(0) == '$') "$" + n else n
  }

  private[utils] def encodeClassFullNameIdent(d: ClassDescriptor)(implicit pos: Position): Ident =
    Ident(encodeClassFullName(d))

  def encodeClassName(className: String, suffix: String): String = {
    val parts = className.split('.').toList
    val (pack, clazz) = parts.partition(_.head.isLower)
    val name = (if (pack.nonEmpty) pack.mkString("", ".", ".") else "") +
      (if (clazz.length > 1) clazz.mkString("$") else clazz.head)
    val n = if (name == "kotlin.Any") "java.lang.Object" else name
    Definitions.encodeClassName(n + suffix)
  }

  private[utils] def encodeClassFullName(d: ClassDescriptor): String = {
    val suffix = if (isCompanionObject(d) || isObject(d)) "$" else ""
    val className = getFqName(d).asString()
    encodeClassName(className, suffix)
  }

  private[utils] def encodeMethodIdent(d: CallableDescriptor, reflProxy: Boolean = false)(implicit pos: Position): Ident =
    Ident(encodeMethodName(d, reflProxy), Some(d.getName.asString()))

  private[utils] def encodeMethodName(d: CallableDescriptor, reflProxy: Boolean = false): String =
    encodeMethodNameInternal(d, reflProxy).mkString

  private def encodeMethodNameInternal(d: CallableDescriptor, reflProxy: Boolean = false, inRTClass: Boolean = false): Seq[String] = {
    val name = encodeMemberNameInternal(d.getName.asString())
    def privateSuffix(cl: Option[ClassDescriptor]) = cl.fold("") { c =>
      if (c.getKind == INTERFACE && !c.isImpl) encodeClassFullName(c)
      else getAllSuperclassesWithoutAny(c).asScala.count(_.getKind != INTERFACE).toString
    }
    //TODO: Only function in classes supported...
    val owner: Option[ClassDescriptor] = d.getContainingDeclaration match {
      case c: ClassDescriptor => Some(c)
      case t: TypeAliasDescriptor => Some(t.getClassDescriptor)
      case _: LazyPackageDescriptor | _: LazyJavaPackageFragment => Option(getContainingClass(d))

      case x => throw new Error(s"${getClass.toString}: Not supported yet: $x")
    }
    val isPrivate = d.getVisibility == Visibilities.PRIVATE
    val encodedName =
      if (isInit(name)) "init" + InnerSep
      else if (isPrivate) encodeName(name) + OuterSep + "p" + privateSuffix(owner)
      else encodeName(name)
    val paramsString = makeParamsString(d, reflProxy, inRTClass)
    Seq(encodedName, paramsString)
  }

  private def makeParamsString(d: CallableDescriptor, reflProxy: Boolean, inRTClass: Boolean): String = {
    val tpes = d.getValueParameters.asScala
    val paramTypeNames0 = tpes.map(_.getReturnType.toJsInternal)
    //    val hasExplicitThisParameter =
    //      inRTClass || isScalaJSDefinedJSClass(sym.owner)
    //        val paramTypeNames =
    //          if (!hasExplicitThisParameter) paramTypeNames0
    //          else internalName(sym.owner.toTypeConstructor) :: paramTypeNames0
    val paramAndResultTypeNames =
    if (isInit(d.getName.asString())) paramTypeNames0
    else if (reflProxy) paramTypeNames0 :+ ""
    else paramTypeNames0 :+ d.getReturnType.toJsInternal
    paramAndResultTypeNames.mkString(OuterSep, OuterSep, "")
  }

  private def encodeMemberNameInternal(s: String): String = s.replace("_", "$und")

}
