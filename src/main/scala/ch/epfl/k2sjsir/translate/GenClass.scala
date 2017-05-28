package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.ClassKind.OBJECT
import org.jetbrains.kotlin.descriptors.{ClassConstructorDescriptor, ClassDescriptor, PropertyDescriptor}
import org.jetbrains.kotlin.js.translate.utils.PsiUtils.getPrimaryConstructorParameters
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.utils.BindingUtils._
import org.jetbrains.kotlin.psi._
import org.jetbrains.kotlin.resolve.DescriptorUtils
import org.jetbrains.kotlin.resolve.calls.callUtil.CallUtilKt
import org.jetbrains.kotlin.resolve.descriptorUtil.DescriptorUtilsKt._
import org.scalajs.core.ir.ClassKind
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.{AnyType, ClassType, NoType}

import scala.collection.JavaConverters._
import scala.collection.immutable.{List, Nil}

case class GenClass(d: KtClassOrObject)(implicit val c: TranslationContext) extends Gen[KtClassOrObject] {

  private val desc = getClassDescriptor(c.bindingContext(), d)
  private val optimizerHints = OptimizerHints.empty
  private val superClass = getSuperClassOrAny(desc)
  private val interfaces = getSuperInterfaces(desc).asScala

  override def tree: ClassDef = {
    val idt = desc.toJsClassIdent
    val kind = if (isModule(desc)) ClassKind.ModuleClass else ClassKind.Class
    val jsNativeLoadSpec = None

    val defs : List[Tree] = d.getDeclarations.asScala.collect {
      case p : KtProperty => GenProperty(p).withGetterAndSetter
      case _: KtClassInitializer | _: KtSecondaryConstructor | _: KtClassOrObject =>
        /**
          * This is a special case, all those element are either used in the constructor (for property init) or
          * by the lowering at the top level (nested classes for instance)
          * Nested classes are handle with lowering, but they remain as declaration in the top class
          */
        List()
      case decl => Seq(GenDeclaration(decl).tree)
    }.flatten.toList

    /**
      * By default constructor parameters are local to constructor, if they are marked as val we need to create a
      * property to export them to the rest of the class
      */
    val paramsInit = getPrimaryConstructorParameters(d)
      .asScala
      .map(getPropertyDescriptorForConstructorParameter(c.bindingContext(), _))
      .filter(_ != null)
      .flatMap(p => {
        val name =  p.toJsIdent
        val tpe = p.getType.toJsType
        FieldDef(static = false, name, tpe, mutable = p.getSetter != null)  ::
          Option(p.getGetter).map(get => GenProperty.getter(get)).toList ++
          Option(p.getSetter).map(set => GenProperty.setter(set)).toList
      }).toList

    val hasMain = defs.exists {
      case MethodDef(_, Ident("main__V", _), _, _, _) => true
      case _ => false
    }
    val constructors = genConstructors
    val allDefs = paramsInit ++ defs.toList ++ constructors
    ClassDef(idt, kind, Some(superClass.toJsClassIdent), interfaces.map(_.toJsClassIdent).toList, jsNativeLoadSpec, allDefs)(optimizerHints)
  }

  private def isModule(c: ClassDescriptor): Boolean = c.getKind == OBJECT

  private def genConstructors : Seq[Tree] = {
    val constructors = d.getSecondaryConstructors.asScala.map(genSecondaryConstructor).toList
    genPrimaryConstructor :: constructors
  }

  private def genSecondaryConstructor(k: KtSecondaryConstructor) : Tree = {
      val callSuper = {
        val delegationCall = CallUtilKt.getResolvedCall(k.getDelegationCall, c.bindingContext())
        val callDesc = delegationCall.getResultingDescriptor
        val args = delegationCall.getValueArgumentsByIndex.asScala
          .map(x => GenExpr(x.getArguments.get(0).getArgumentExpression).tree).toList
        ApplyStatically(This()(desc.toJsClassType), desc.toJsClassType, callDesc.toJsMethodIdent, args)(NoType)
      }

      val constrDesc = getDescriptorForElement(c.bindingContext(), k).asInstanceOf[ClassConstructorDescriptor]

      val body = Block(callSuper :: GenBody(k.getBodyExpression).treeOption.toList)
      val args = constrDesc.getValueParameters.asScala.map(_.toJsParamDef).toList
      MethodDef(static = false, constrDesc.toJsMethodIdent, args, NoType, Some(body))(optimizerHints, None)
  }

  private def genPrimaryConstructor : Tree = {
    /**
      * Declarations (for properties) must be in the primary constructors
      * Secondary constructors will call primary one
      */

    /**
      * If primary constructor params are marked as 'val', we have to create a class field to "export" them
      * out of the constructor scope, this can only be done in primary constructors
      */
    val paramsInit = getPrimaryConstructorParameters(d)
      .asScala
      .map(getPropertyDescriptorForConstructorParameter(c.bindingContext(), _))
      .filter(_ != null)
      .map(p => {
        val name =  p.toJsIdent
        val tpe = p.getType.toJsType
        Assign(Select(This()(desc.toJsClassType), name)(tpe), VarRef(name)(tpe))
      }).toList

    val declsInit = d.getDeclarations.asScala.collect {
      case p: KtProperty =>
        val expr = GenExpr(p.getDelegateExpressionOrInitializer).tree
        Assign(Select(This()(desc.toJsClassType), Ident(p.getName))(expr.tpe), expr)
      case i: KtClassInitializer => GenBody(i.getBody).tree
    }.toList

    val superCall = Option(getSuperCall(c.bindingContext(), d)) match {
      case Some(call) =>
        val name =  call.getResultingDescriptor.asInstanceOf[ClassConstructorDescriptor].toJsMethodIdent
        val params = call.getValueArgumentsByIndex.asScala
          .map(x => GenExpr(x.getArguments.get(0).getArgumentExpression).tree) //TODO: why list of ValueArgument
        val tpe = superClass.toJsClassType
        ApplyStatically(This()(tpe), tpe, name, params.toList)(NoType)
      case None => // We have no superclass, hence we need to call init on Object
        val o = ClassType("O")
        ApplyStatically(This()(o), o, Ident("init___"), List())(NoType)
    }

    val primary = desc.getUnsubstitutedPrimaryConstructor
    val args = primary.getValueParameters.asScala.map(_.toJsParamDef).toList

    val stats = Block(superCall :: paramsInit ++ declsInit)
    MethodDef(static = false, primary.toJsMethodIdent, args, NoType, Some(stats))(optimizerHints, None)
  }
}
