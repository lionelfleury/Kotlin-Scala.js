package ch.epfl.k2sjsir.utils

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.builtins.BuiltInsPackageFragment
import org.jetbrains.kotlin.com.intellij.psi.PsiFile
import org.jetbrains.kotlin.descriptors.ClassKind.INTERFACE
import org.jetbrains.kotlin.descriptors._
import org.jetbrains.kotlin.fileClasses.JvmFileClassUtil
import org.jetbrains.kotlin.load.java.`lazy`.descriptors.LazyJavaPackageFragment
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.resolve.{DescriptorToSourceUtils, DescriptorUtils}
import org.jetbrains.kotlin.resolve.DescriptorUtils._
import org.jetbrains.kotlin.resolve.`lazy`.descriptors.LazyPackageDescriptor
import org.jetbrains.kotlin.resolve.descriptorUtil.DescriptorUtilsKt.getAllSuperclassesWithoutAny
import org.jetbrains.kotlin.resolve.source.PsiSourceFile
import org.jetbrains.kotlin.serialization.js.KotlinJavascriptPackageFragment
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

  private[utils] def encodeClassFullNameIdent(d: ClassDescriptor, kotlinNaming: Boolean = false)(
      implicit pos: Position): Ident =
    Ident(encodeClassFullName(d, kotlinNaming), Some(d.getName.asString()))

  def encodeClassName(className: String, suffix: String): String = {
    val parts = className.split('.').toList
    val (pack, clazz) = parts.partition(_.head.isLower)
    val name = {
      if (pack.nonEmpty && clazz.nonEmpty)
        pack.mkString("", ".", ".")
      else if (pack.nonEmpty)
        pack.slice(0, pack.length - 1).mkString("", ".", ".")
      else
        ""
    } + {
      if (clazz.length > 1)
        clazz.mkString("$")
      else if (clazz.length == 1)
        clazz.head
      else
        pack.last
    }

    val n = {
      if (name == "kotlin.Any") "java.lang.Object"
      else if (name == "kotlin.Enum") "java.lang.Enum"
      else name
    }

    Definitions.encodeClassName(n + suffix)
  }

  private[utils] def encodeClassFullName(d: ClassDescriptor, kotlinNaming: Boolean = false): String = {
    val suffix = {
      if (isCompanionObject(d) || isObject(d) || (kotlinNaming && d.isEnumClass)) "$"
      else if (kotlinNaming && d.isInterface) "$DefaultImpls"
      else ""
    }

    val className = d.getNameEvenIfAnon
    encodeClassName(className, suffix)
  }

  private[utils] def encodeMethodIdent(
      d: CallableDescriptor,
      reflProxy: Boolean = false,
      kotlinNaming: Boolean = false)(implicit pos: Position): Ident =
    Ident(encodeMethodName(d, reflProxy, kotlinNaming = kotlinNaming), Some(d.getName.asString()))

  private[utils] def encodeMethodName(
      d: CallableDescriptor,
      reflProxy: Boolean = false,
      kotlinNaming: Boolean = false)(implicit pos: Position): String =
    encodeMethodNameInternal(d, reflProxy, inRTClass = false, kotlinNaming = kotlinNaming).mkString

  private def encodeMethodNameInternal(
      d: CallableDescriptor,
      reflProxy: Boolean = false,
      inRTClass: Boolean = false,
      kotlinNaming: Boolean = false)(implicit pos: Position): Seq[String] = {
    val stringName = d.getNameEvenIfAnon
    val name = encodeMemberNameInternal(stringName)
    def privateSuffix(cl: Option[ClassDescriptor]) = cl.fold("") { c =>
      if (c.getKind == INTERFACE && !c.isActual) encodeClassFullName(c)
      else
        getAllSuperclassesWithoutAny(c).asScala
          .count(_.getKind != INTERFACE)
          .toString
    }
    val owner: Option[ClassDescriptor] = d.getContainingDeclaration match {
      case c: ClassDescriptor     => Some(c)
      case t: TypeAliasDescriptor => Some(t.getClassDescriptor)
      case _: LazyPackageDescriptor | _: LazyJavaPackageFragment |
          _: BuiltInsPackageFragment /*| _: KotlinJavascriptPackageFragment*/ =>
        Option(getContainingClass(d))
      case sf: SimpleFunctionDescriptor =>
        Option(DescriptorUtils.getContainingClass(sf))
      case x =>
        throw new Error(s"${getClass.toString}: Not supported yet: $x")
    }
    val isPrivate = d.getVisibility == Visibilities.PRIVATE
    val encodedName =
      if (isInit(name)) "init" + InnerSep
      else if (isPrivate)
        encodeName(name) + OuterSep + "p" + privateSuffix(owner)
      else encodeName(name)
    val paramsString = makeParamsString(d, reflProxy, inRTClass, kotlinNaming = kotlinNaming)
    Seq(encodedName, paramsString)
  }

  private def makeParamsString(
      d: CallableDescriptor,
      reflProxy: Boolean,
      inRTClass: Boolean,
      kotlinNaming: Boolean = false)(implicit pos: Position): String = {
    val owner = d.getContainingDeclaration match {
      case c: ClassDescriptor => Some(c)
      case _ => None
    }

    val paramsIntf = owner.fold("")(c => if (kotlinNaming && c.isInterface) c.toJsClassName else "")
    val params0 = d.getOriginal.getValueParameters.asScala.map(_.toJsInternal)
    val x = d.getExtensionReceiverParameter
    val params1 =
      if (x == null) params0 else x.getReturnType.toJsInternal +: params0
    val params2 =
      if (isInit(d.getName.asString())) params1
      else if (reflProxy) params1 :+ ""
      else params1 :+ d.getOriginal.getReturnType.toJsInternal

    val params = if (paramsIntf != "") paramsIntf +: params2 else params2
    params.mkString(OuterSep, OuterSep, "")
  }

  def encodeApplyLambda(desc: CallableDescriptor)(implicit pos: Position): Ident = {
    val retType = desc.getReturnType.toJsInternal

    val concatType =
      desc.getValueParameters.asScala.map(_ => "__O").mkString("")

    Ident(s"apply${concatType}__O")
  }

  def encodeWithSourceFile(d: DeclarationDescriptor): String = {
    val file: KtFile = DescriptorToSourceUtils.getContainingFile(d)

    val name = JvmFileClassUtil
      .getFileClassInfoNoResolve(file)
      .getFileClassFqName

    NameEncoder.encodeClassName(name.asString(), "")
  }

  private def encodeMemberNameInternal(s: String): String =
    s.replace("_", "$und")

}
