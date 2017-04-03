package ch.epfl.k2sjsir.utils

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.builtins.BuiltInsPackageFragment
import org.jetbrains.kotlin.descriptors.ClassKind.INTERFACE
import org.jetbrains.kotlin.descriptors._
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
    val n0 = nonValid.foldLeft(name)((n, r) => n.replace(r, ""))
    val n = if (n0.isEmpty) "set" else n0
    if (isKeyword(n) || n(0).isDigit || n(0) == '$') "$" + n else n
  }

  private[utils] def encodeClassFullNameIdent(d: ClassDescriptor)(implicit pos: Position): Ident =
    Ident(encodeClassFullName(d), Some(d.getName.asString()))

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

  private[utils] def encodeMethodName(d: CallableDescriptor, reflProxy: Boolean = false)(implicit pos: Position): String =
    encodeMethodNameInternal(d, reflProxy).mkString

  private def encodeMethodNameInternal(d: CallableDescriptor, reflProxy: Boolean = false, inRTClass: Boolean = false)(implicit pos: Position): Seq[String] = {
    val name = encodeMemberNameInternal(d.getName.asString())
    def privateSuffix(cl: Option[ClassDescriptor]) = cl.fold("") { c =>
      if (c.getKind == INTERFACE && !c.isImpl) encodeClassFullName(c)
      else getAllSuperclassesWithoutAny(c).asScala.count(_.getKind != INTERFACE).toString
    }
    val owner: Option[ClassDescriptor] = d.getContainingDeclaration match {
      case c: ClassDescriptor => Some(c)
      case t: TypeAliasDescriptor => Some(t.getClassDescriptor)
      case _: LazyPackageDescriptor | _: LazyJavaPackageFragment
           | _: BuiltInsPackageFragment => Option(getContainingClass(d))
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

  private def makeParamsString(d: CallableDescriptor, reflProxy: Boolean, inRTClass: Boolean)(implicit pos: Position): String = {
    val tpes = d.getValueParameters.asScala
    val params0 = tpes.map {
      case dd: ValueParameterDescriptor if isVarArg(dd) => "A"+dd.getVarargElementType.toJsInternal
      case e => e.getReturnType.toJsInternal
    }
    val x = d.getExtensionReceiverParameter
    val params1 = if (x == null) params0 else x.getReturnType.toJsInternal +: params0
    val params =
      if (isInit(d.getName.asString())) params1
      else if (reflProxy) params1 :+ ""
      else params1 :+ d.getReturnType.toJsInternal
    params.mkString(OuterSep, OuterSep, "")
  }

  private def encodeMemberNameInternal(s: String): String = s.replace("_", "$und")

}
