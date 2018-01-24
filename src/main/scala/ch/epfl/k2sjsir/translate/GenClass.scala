package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.utils.GenClassUtils._
import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.ClassConstructorDescriptor
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.utils.BindingUtils
import org.jetbrains.kotlin.js.translate.utils.BindingUtils._
import org.jetbrains.kotlin.js.translate.utils.PsiUtils.getPrimaryConstructorParameters
import org.jetbrains.kotlin.psi._
import org.jetbrains.kotlin.resolve.calls.callUtil.CallUtilKt
import org.jetbrains.kotlin.resolve.descriptorUtil.DescriptorUtilsKt
import org.jetbrains.kotlin.resolve.descriptorUtil.DescriptorUtilsKt._
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.{ClassType, IntType, NoType, StringType}
import org.scalajs.core.ir.{ClassKind, Definitions, Trees}

import scala.collection.JavaConverters._
import scala.collection.immutable.List


case class GenClass(d: KtClassOrObject)(implicit val c: TranslationContext) extends IRNodeGen[KtClassOrObject, ClassDef] {

  private val desc = getClassDescriptor(c.bindingContext(), d)
  private val optimizerHints = OptimizerHints.empty
  private val kind = desc.toJsClassKind
  private val superClass = if(desc.isInterface) None else Some(DescriptorUtilsKt.getSuperClassOrAny(desc))
  private val interfaces = getSuperInterfaces(desc).asScala

  override def tree: ClassDef = {
    val idt = desc.toJsClassIdent
    val jsNativeLoadSpec = None

    /*
    For interfaces :
      => there are no constructors, we therefore need to generate a DefaultImpl file which will
      contain all default implementations provided in the interface.
      These implementation take a class instance as parameter

    - Generate the complete class abstracted
    - Generate all implementations in separate file containing
      - static declarations of the implementations
      - first parameter should be instance of the interface
      - calls to this keyword matching the interface should be replaced by this instance
      - naming convention : <interface_name>$DefaultImpls

    For classes implementing interfaces :
    (Kotlin generates fake descriptors in order to generate bridges)
    - Generate all overridden methods and variables as usual
    - Generate missing methods (from interfaces) by calling the default implementation of the interfaces
      see org.jetbrains.kotlin.backend.common.bridges


    For Enum classes :
      => If it's a ENUM_CLASS
        - Extends java.lang.Enum
        - Static initialization of the fields -> create companion object ?
          For example :
            public static final /* enum */ MyEnum ONE;
            public static final /* enum */ MyEnum TWO;
            public static final /* enum */ MyEnum ZERO;
            private static final /* synthetic */ MyEnum[] $VALUES;

            static {
              MyEnum[] arrmyEnum = new MyEnum[3];
              MyEnum[] arrmyEnum2 = arrmyEnum;
              arrmyEnum[0] = MyEnum.ONE = new ONE("ONE", 0);
              arrmyEnum[1] = MyEnum.TWO = new TWO("TWO", 1);
              arrmyEnum[2] = MyEnum.ZERO = new ZERO("ZERO", 2);
              $VALUES = arrmyEnum;
            }
        - Add a values() function :
            public static MyEnum[] values() {
              return (MyEnum[])$VALUES.clone();
            }
        - Add a valueOf(string) function :
            public static MyEnum valueOf(String string) {
              return Enum.valueOf(MyEnum.class, string);
            }

        - (Private empty constructor)
        - One anonymous class per value

      => If it's a ENUM_ENTRY
        - Standard class extending the ENUM_CLASS

      => For references in the code --> generate a static call to the companion ?

      MyEnum (enum_class)
        - all standard usual definitions
      MyEnum$Companion
        - all static definitions
      MyEnum$ENTRY (enum_entry)
        - generated with lowering

     */

    /**
      * By default constructor parameters are local to constructor, if they are marked as val we need to create a
      * property to export them to the rest of the class
      */
    val paramsInit = d.getPrimaryConstructorParameters.asScala.flatMap { param =>
      val propDesc = getPropertyDescriptorForConstructorParameter(c.bindingContext(), param)

      if (propDesc == null)
        Nil
      else {
        val isMutable = propDesc.getSetter != null

        if (propDesc != null) {
          val name = propDesc.toJsIdent
          val tpe = propDesc.getType.toJsType

          val fd = List(FieldDef(static = false, name, tpe, mutable = isMutable))
          val g = List(GenProperty.getter(propDesc))
          val s = if (isMutable) List(GenProperty.setter(propDesc)) else Nil

          fd ++ g ++ s

        } else
          Nil

      }
    }.toList

    val constructors = genConstructors
    val memberDefs = d.getMemberDefinitions
    val bridges = d.getInterfaceBridges

    val allDefs = paramsInit ++ memberDefs ++ bridges ++ constructors

    val sprCls = superClass.fold(None: Option[Trees.Ident])(x => Some(x.toJsClassIdent))

    val classDef = ClassDef(idt, kind, None, sprCls, interfaces.map(_.toJsClassIdent).toList, None, jsNativeLoadSpec, allDefs, Nil)(optimizerHints)

    classDef
  }

  /**
    * Creates a new class definition containing static definitions of the default implementations
    * of the currently compiled interface.
    * These methods will be called inside the bridges generated in the inheriting classes
    *
    * @return A fresh class def containing the default interface implementations
    */
  def treeDefaultImpls: Option[ClassDef] = {
    val impls = d.getDefaultImplementations


    if (impls.isEmpty) return None

    val kind = ClassKind.Class
    val name = desc.toJsDefaultImplIdent
    val superClass = Some(Ident(Definitions.ObjectClass))

    val defs: List[MemberDef] = impls.collect {
      case p: KtProperty =>
        GenProperty(p).withGetterAndSetter

      case decl =>
        Seq(GenDeclaration(decl).tree)

    }.flatten

    val classDef = ClassDef(name, kind, None, superClass, List(), None, None, defs, Nil)(OptimizerHints.empty)

    Option(classDef)
  }

  def treeEnumCompanion: Option[ClassDef] = {
    if (!desc.isEnumClass) return None

    val defs: List[MemberDef] = d.getEnumDefinitions

    val ctor: MemberDef = d.getEnumCompanionConstructor

    val name = desc.toJsEnumCompanionIdent
    val superClass = Some(Ident("O"))

    val classDef = ClassDef(name, ClassKind.ModuleClass, None, superClass, List(), None, None, defs ++ List(ctor), Nil)(OptimizerHints.empty)

    Option(classDef)
  }

  private def genConstructors : Seq[MethodDef] = {
    val constructors = d.getSecondaryConstructors.asScala.map(genSecondaryConstructor).toList
    genPrimaryConstructor.toList ++ constructors
  }

  private def genSecondaryConstructor(k: KtSecondaryConstructor) : MethodDef = {
      /*
       * Need to call the primary constructor in each secondary constructor
       */
      val callSuper = {
        val delegationCall = CallUtilKt.getResolvedCall(k.getDelegationCall, c.bindingContext())
        val callDesc = delegationCall.getResultingDescriptor
        val args = delegationCall.getValueArgumentsByIndex.asScala
          .map(x => GenExpr(x.getArguments.get(0).getArgumentExpression).tree).toList
        val receiver = genThisFromContext(desc.toJsClassType)
        ApplyStatically(receiver, desc.toJsClassType, callDesc.toJsMethodIdent, args)(NoType)
      }

      val constrDesc = getDescriptorForElement(c.bindingContext(), k).asInstanceOf[ClassConstructorDescriptor]

      val body = Block(callSuper :: GenBody(k.getBodyExpression).treeOption.toList)
      val args = constrDesc.getValueParameters.asScala.map(_.toJsParamDef).toList
      MethodDef(static = false, constrDesc.toJsMethodIdent, args, NoType, Some(body))(optimizerHints, None)
  }

  private def genPrimaryConstructor : Option[MethodDef] = {
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
      .filter(getPropertyDescriptorForConstructorParameter(c.bindingContext(), _) != null)
      .map(param => {
        val paramDesc = BindingUtils.getDescriptorForElement(c.bindingContext(), param)
        val propDesc = getPropertyDescriptorForConstructorParameter(c.bindingContext(), param)

        val tpe = propDesc.getType.toJsType
        val rcv = genThisFromContext(desc.toJsClassType)

        Assign(Select(rcv, propDesc.toJsIdent)(tpe), VarRef(paramDesc.toJsIdent)(tpe))
      }).toList

    val declsInit: List[Tree] = d.getDeclarations.asScala
        .collect {
          case p: KtProperty if p.hasDelegateExpressionOrInitializer =>
            val propDesc = BindingUtils.getPropertyDescriptor(c.bindingContext(), p)
            val initExpr = p.getDelegateExpressionOrInitializer
            val expr = GenExpr(initExpr).tree
            val rcv = genThisFromContext(desc.toJsClassType)

            Assign(Select(rcv, propDesc.toJsIdent)(propDesc.getType.toJsType), expr)

          case i: KtClassInitializer =>
            GenBody(i.getBody).tree

    }.toList

    val primary = desc.getUnsubstitutedPrimaryConstructor

    val superCall = Option(getSuperCall(c.bindingContext(), d)) match {
      case Some(call) =>
        val name =  call.getResultingDescriptor.asInstanceOf[ClassConstructorDescriptor].toJsMethodIdent
        val params = call.getValueArgumentsByIndex.asScala
          .map(x => GenExpr(x.getArguments.get(0).getArgumentExpression).tree)
        val tpe = superClass.get.toJsClassType
        val rcv = genThisFromContext(tpe)
        ApplyStatically(rcv, tpe, name, params.toList)(NoType)
      case None => // We have no superclass, hence we need to call init on Object
        val (idt, clsTpe, params) = {
          if (superClass.nonEmpty && superClass.get.toJsClassType != ClassType("O")) {
            assert(primary != null, "Primary constructor should exists as there is a superclass")
            val superCls = superClass.get

            val isEnumSpecial = superCls.isEnumClass || superCls.isJLEnum

            val idt = primary.toJsMethodIdent match {
              case i: Ident =>
                if(isEnumSpecial)
                  d.genEnumCtorIdent(superCls)
                else
                  i
            }

            val params = {
              if(isEnumSpecial)
                List(VarRef(Ident("_name"))(ClassType("T")), VarRef(Ident("_ordinal"))(IntType))
              else
                Nil
            }

            (idt, superCls.toJsClassType, params)

          } else {
            val o = ClassType("O")
            val idt = Ident("init___")
            (idt, o, Nil)

          }
        }
        ApplyStatically(genThisFromContext(clsTpe), clsTpe, idt, params)(NoType)
    }

    if(primary != null) {
      val originArgs = primary.getValueParameters.asScala.map(_.toJsParamDef).toList
      val enumArgs = {
        if (desc.isEnumClass || desc.isEnumEntry)
          List(
            ParamDef(Ident("_name"), ClassType("T"), mutable = false, rest = false),
            ParamDef(Ident("_ordinal"), IntType, mutable = false, rest = false)
          )
        else
          Nil
      }

      val args = originArgs ++ enumArgs

      val ctorIdent = primary.toJsMethodIdent match {
        case i: Ident =>
          if (desc.isEnumClass || desc.isEnumEntry)
            d.genEnumCtorIdent(desc)
          else
            i
      }

      val stats = Block(superCall :: paramsInit ++ declsInit)
      Some(MethodDef(static = false, ctorIdent, args, NoType, Some(stats))(optimizerHints, None))
    } else None
  }

}
