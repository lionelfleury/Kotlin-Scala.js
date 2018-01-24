package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors._
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.utils.BindingUtils
import org.jetbrains.kotlin.psi.{KtProperty, KtPropertyAccessor}
import org.jetbrains.kotlin.resolve.DescriptorUtils
import org.jetbrains.kotlin.resolve.DescriptorUtils._
import org.jetbrains.kotlin.types.KotlinType
import org.scalajs.core.ir.Position
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.NoType

import scala.collection.JavaConverters._


case class GenProperty(d: KtProperty)(implicit val c: TranslationContext) extends IRNodeGen[KtProperty, FieldDef] {

  import GenProperty._

  private val desc = BindingUtils.getPropertyDescriptor(c.bindingContext(), d)


  override def tree: FieldDef = {
    val idt = desc.toJsIdent
    val static = DescriptorUtils.isStaticDeclaration(desc)

    val tpe = desc.getType.toJsType

    FieldDef(static, idt, tpe, desc.isVar)
  }

  def withGetterAndSetter: List[MemberDef] = {
    val cd = desc.getContainingDeclaration match {
      case c: ClassDescriptor => c
      case _ => null
    }

    // FIXME: Generating a backing field should only happen if one of the defaults is used, or if the field keyword is used
    val t = {
      if ((d.isVar || d.hasInitializer) && cd != null && !cd.isInterface)
        List(tree)
      else
        Nil
    }

    t ++ genGetterAndSetter(isAbstract = false)
  }

  def withAbstractGetterAndSetter: List[MethodDef] = {
    genGetterAndSetter(isAbstract = true)
  }

  private def genGetterAndSetter(isAbstract: Boolean): List[MethodDef] = {
    /*
    1. Do not generate a FieldDef if there is no body (this has to be done inside the implementing class)
    2. Generate in every case an (abstract) getter (and an (abstract) setter if it's a var)
    3. In GenClass create a DefaultImpls file containing all defined implementations
     */

    setProp(d)

    val g = List(getter(d, isAbstract))

    val s = if (d.isVar) List(setter(d, isAbstract)) else Nil

    g ++ s
  }
}

object GenProperty {
  var prop: Option[KtProperty] = None

  private def setProp(p: KtProperty): Unit = {
    prop = Option(p)
  }

  /**
    * If no getter was defined, generates the default implementation.
    * Otherwise generates a getter provided by the user, for example :
    *
    * val name: String
    *  get() = "some constant value for name"
    *
    */
  private def getter(p: KtProperty, isAbstract: Boolean = false)(implicit c: TranslationContext, pos: Position): MethodDef = {
    val propAccessor = p.getGetter
    val desc: PropertyDescriptor = BindingUtils.getPropertyDescriptor(c.bindingContext(), p)
    val propGetterDesc = desc.getGetter

    getter(propAccessor, desc, propGetterDesc, isAbstract)
  }

  private[translate] def getter(desc: PropertyDescriptor)(implicit c: TranslationContext, pos: Position): MethodDef = {
    val propertyAccessor = null
    val propertyGetterDescriptor = desc.getGetter

    getter(propertyAccessor, desc, propertyGetterDescriptor, false)
  }

  private def getter(propAccessor: KtPropertyAccessor, desc: PropertyDescriptor, propGetterDesc: PropertyGetterDescriptor, isAbstract: Boolean)(implicit c: TranslationContext, pos: Position): MethodDef = {
    assert(propGetterDesc != null)

    val rtpe = desc.getReturnType.toJsType

    val methodIdent = propGetterDesc.toJsMethodDeclIdent
    val static = isStaticAccessor(desc) && !isAbstract

    val body = {
      if (!isAbstract) {

        if (propAccessor != null && propAccessor.hasBody)
          Option(GenBody(propAccessor.getBodyExpression).tree)
        else
          Option(genSelectForAccessor(desc))

      } else {
        None
      }
    }

    val args = getInterfaceReceiverParameter(desc).toList


    MethodDef(static, methodIdent, args, rtpe, body)(OptimizerHints.empty, None)
  }

  /**
    * Generate a default setter or a custom one, if defined by the user, such as :
    * var myVar: Int = 0
    *   set(value) {
    *     if (value >= 0)
    *       field = value
    *     else
    *       field = 0
    *   }
    */
  private def setter(propAccessor: KtPropertyAccessor, desc: PropertyDescriptor, propSetterDesc: PropertySetterDescriptor, isAbstract: Boolean)(implicit c: TranslationContext, pos: Position): MethodDef = {
    assert(propSetterDesc != null)

    val rtpe = NoType // Kotlin setters always return Unit
    val isStatic = isStaticAccessor(desc) && !isAbstract
    val propTpe = desc.getType.toJsType
    val methodIdent = propSetterDesc.toJsMethodDeclIdent
    val setterIdent = propSetterDesc.getValueParameters.get(0).toJsIdent
    val params = getInterfaceReceiverParameter(desc).toList ++ List(ParamDef(setterIdent, desc.getType.toJsType, mutable = false, rest = false))

    val body = {
      if (!isAbstract) {
        if (propAccessor != null && propAccessor.hasBody)
          Option(GenBody(propAccessor.getBodyExpression).tree)
        else
          Some(Assign(genSelectForAccessor(desc), VarRef(setterIdent)(propTpe)))

      } else
        None

    }

    MethodDef(static = isStatic, methodIdent, params, rtpe, body)(OptimizerHints.empty, None)
  }

  private def setter(p: KtProperty, isAbstract: Boolean = false)(implicit c: TranslationContext, pos: Position): MethodDef = {
    val propAccessor = p.getSetter
    val desc = BindingUtils.getPropertyDescriptor(c.bindingContext(), p)
    val propSetterDesc = desc.getSetter

    setter(propAccessor, desc, propSetterDesc, isAbstract)
  }

  private[translate] def setter(desc: PropertyDescriptor)(implicit c: TranslationContext, pos: Position): MethodDef = {
    val propertyAccessor = null
    val propertySetterDescriptor = desc.getSetter

    setter(propertyAccessor, desc, propertySetterDescriptor, isAbstract = false)
  }


  /**
    * Generates a Select Tree in the context of accessors
    *
    * @param desc
    * @param pos
    * @return
    */
  private def genSelectForAccessor(desc: PropertyDescriptor)(implicit pos: Position): Tree = {
    val ident = desc.toJsIdent
    val slctTpe = desc.getType.toJsType

    val clsTpe = if (!desc.isTopLevel) {
      val tpe = desc.getDispatchReceiverParameter.getType
      DescriptorUtils.getClassDescriptorForType(tpe).toJsClassType

    } else {
      val file = prop.getOrElse(throw new Exception("Declaration is in root package, you must set the KtProperty before the call")).getContainingKtFile
      file.toJsClassType
    }

    val rcv = genThisFromContext(clsTpe, desc)
    Select(rcv, ident)(slctTpe)
  }

  private def isStaticAccessor(propDesc: PropertyDescriptor): Boolean = {
    val dispatchReceiver = Option(propDesc.getDispatchReceiverParameter)
    val tpe = dispatchReceiver.fold(null: KotlinType)(dr => dr.getType)
    val cd = dispatchReceiver.fold(null: ClassDescriptor)(_ => DescriptorUtils.getClassDescriptorForType(tpe))

    cd != null && cd.isInterface
  }

  private def getInterfaceReceiverParameter(desc: PropertyDescriptor)(implicit pos: Position): Option[ParamDef] = {

    val dispatchReceiver = Option(desc.getDispatchReceiverParameter)
    val tpe = dispatchReceiver.fold(null: KotlinType)(dr => dr.getType)
    val cd = dispatchReceiver.fold(null: ClassDescriptor)(_ => DescriptorUtils.getClassDescriptorForType(tpe))

    if (cd != null && isInterface(cd))
      Option(ParamDef(Ident("$this"), cd.toJsClassType, false, false))
    else
      None
  }
}
