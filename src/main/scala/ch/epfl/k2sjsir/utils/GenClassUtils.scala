package ch.epfl.k2sjsir.utils

import ch.epfl.k2sjsir.lower.SJSIRLower
import ch.epfl.k2sjsir.translate.{GenDeclaration, GenEnumEntry, GenProperty}
import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.{ClassDescriptor, PropertyDescriptor}
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.utils.BindingUtils
import org.jetbrains.kotlin.js.translate.utils.BindingUtils.{getFunctionDescriptor, getPropertyDescriptor}
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.psi._
import org.jetbrains.kotlin.resolve.descriptorUtil.DescriptorUtilsKt
import org.jetbrains.kotlin.resolve.{DescriptorToSourceUtils, DescriptorUtils}
import org.scalajs.core.ir.{Position, Trees}
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types._

import scala.collection.JavaConverters._
import scala.collection.immutable.List

object GenClassUtils {

  implicit class KtClassOrObjectHelper(clsOrObj: KtClassOrObject)(
      implicit val c: TranslationContext,
      pos: Position) {

    protected val desc = BindingUtils.getClassDescriptor(c.bindingContext(), clsOrObj)

    /* For Enum Classes generation */
    private val enumName = desc.toJsClassType.className

    private val arrayValuesName = "$VALUES"
    private val arrayTpe = ArrayType(ArrayTypeRef(enumName, 1))

    private val valueOfName = "valueOf"
    private val valueOfParamName = "string"
    private val valuesName = "values"

    /**
      * Retrieves a list of all inherited declarations from super interfaces
      *
      * @return The list of all inherited declarations
      */
    def getInterfaceInheritedDeclarations: List[KtDeclaration] =
      _inheritedDeclarations(DescriptorUtilsKt.getSuperInterfaces(desc).asScala.toList, Set())

    /**
      * Builds a set containing signatures of all overridden methods in the current class
      *
      * @return A set of String of the overridden methods signatures
      */
    def getOverriddenDeclarations: Set[String] = {
      val fromPrimaryConstructor = clsOrObj.getPrimaryConstructorParameters.asScala.filter(_.hasModifier(KtTokens.OVERRIDE_KEYWORD))

      (clsOrObj.getDeclarations.asScala ++ fromPrimaryConstructor)
        .filter(x => x.hasModifier(KtTokens.OVERRIDE_KEYWORD))
        .map {
          case nf: KtNamedFunction =>
            BindingUtils.getFunctionDescriptor(c.bindingContext(), nf).getName.asString()

          case p: KtProperty =>
            BindingUtils.getPropertyDescriptor(c.bindingContext(), p).getName.asString()

          case p: KtParameter =>
            BindingUtils.getPropertyDescriptorForConstructorParameter(c.bindingContext(), p).getName.asString()

        }
        .toSet
    }

    /**
      * Retrieves a list of all missing declarations provided in the super interfaces
      * (implicitly this means that theses declarations have a default implementation)
      *
      * @return The list of missing declarations
      */
    def getMissingInterfaceDeclarations: List[KtDeclaration] = {
      lazy val overriddenDeclarations = clsOrObj.getOverriddenDeclarations

      clsOrObj.getInterfaceInheritedDeclarations.collect {
        case p: KtProperty =>
          val pDesc = BindingUtils.getPropertyDescriptor(c.bindingContext(), p)

          if (overriddenDeclarations.contains(pDesc.getName.asString()))
            Nil
          else
            List(p)

        case nf: KtNamedFunction =>
          val nfDesc = BindingUtils.getFunctionDescriptor(c.bindingContext(), nf)
          if (overriddenDeclarations.contains(nfDesc.getName.asString()))
            Nil
          else
            List(nf)

      }.flatten
    }

    /**
      * Retrieves all default implementations for an interface
      * @return The list of default declarations
      */
    def getDefaultImplementations: List[KtDeclaration] = {
      clsOrObj.getDeclarations.asScala
        .collect {
          case p: KtProperty =>
            if (desc.isInterface && p.hasDefinedAccessors) List(p)
            else Nil

          case _: KtClassInitializer | _: KtSecondaryConstructor | _: KtClassOrObject =>
            Nil

          case declaration =>
            if (desc.isInterface) {
              declaration match {
                case nf: KtNamedFunction if nf.hasBody => List(declaration)
                case _                                 => Nil
              }
            } else Nil

        }
        .flatten
        .toList
    }

    /**
      * Generates method bridges for classes with interfaces that provide default implementations
      *
      * For example :
      *
      * def myInterfaceMethodName(myInterfaceMethodArgs) {
      *   MyInterface$DefaultImpls.myInterfaceMethodName(this, myInterfaceMethodArgs)
      * }
      *
      * @return A list of MethodDef bridges
      */
    def getInterfaceBridges: List[MethodDef] = {
      clsOrObj.getMissingInterfaceDeclarations.collect {
        case nf: KtNamedFunction if !desc.isInterface =>
          val bridge = _toBridge(GenDeclaration(nf).tree, nf)

          Seq(bridge)
        // Generate bridges for accessors
        case p: KtProperty if !desc.isInterface =>
          val bridges = GenProperty(p)
            .withGetterAndSetter
            .filter(_.isInstanceOf[MethodDef])
            .map(m => _toBridge(m, p))

          bridges
      }.flatten
    }

    /**
      * Generates all MemberDef for the current class or object. Enum entries are not handled here
      * because they should be inside a companion object of the enum class they belong to.
      *
      * Note : KtClassInitializer, KtSecondaryConstructor and KtClassOrObject are skipped because they
      * are all either used in the constructor (for property init) or by the lowering at the top level (nested classes for instance).
      * Nested classes are handled with lowering, but they remain as declaration in the top class.
      *
      * @return A list of MemberDef corresponding to the member class or object declarations
      */
    def getMemberDefinitions: List[MemberDef] =
      _memberDefinitions(clsOrObj.getDeclarations.asScala.toList)

    /**
      * Generates all MemberDef for the current EnumClass. These MemberDef should be given to a
      * companion object definition since the kotlin definitions are static members.
      *
      * @return
      */
    def getEnumDefinitions: List[Trees.MemberDef] =
      _memberDefinitions(clsOrObj.getDeclarations.asScala.toList, onlyEnumEntries = true) ++
        List(_genValueOfFunction) ++ _genValuesFunction

    def genEnumCtorIdent(descriptor: ClassDescriptor): Ident = {
      val primary = Option(descriptor.getUnsubstitutedPrimaryConstructor)
      val addedSignature = "T__I"

      val ident: Ident = {
        if (primary.nonEmpty) {
          primary.get.toJsMethodIdent match {
            case Ident(name, originalName) if !descriptor.isJLEnum =>
              val separator = if (name.endsWith("_")) "" else "__"
              val newName = name + separator + addedSignature
              Ident(newName, originalName)

            case i => i
          }
        } else {
          val newName = s"init___$addedSignature"
          Ident(newName)
        }
      }

      val args: List[Tree] = {
        val superCall = BindingUtils.getSuperCall(c.bindingContext(), clsOrObj)

        if (primary.nonEmpty)
          Nil//primary.get.getValueParameters.asScala.map(_.toJsParamDef).toList
        else
          Nil
      }

      ident
    }

    def getEnumCompanionConstructor: MemberDef = {
      val entries = clsOrObj.getDeclarations.asScala
      // Initialize the values array --> $VALUES = Array(entries)
      val elems = entries.collect {
        case e: KtEnumEntry =>
          val enumEntryDesc = BindingUtils.getClassDescriptor(c.bindingContext(), e)
          Select(genThisFromContext(desc.toJsEnumCompanionType), enumEntryDesc.toJsIdent)(
            desc.toJsClassType)
      }.toList
      val thisValues = Select(genThisFromContext(desc.toJsEnumCompanionType), Ident(arrayValuesName))(arrayTpe)
      val valuesArray = Assign(thisValues, ArrayValue(arrayTpe, elems))

      // Call constructor on all entries
      var enumCardinality = 0
      val initializations: List[Tree] = clsOrObj.getDeclarations.asScala
        .collect {
          case e: KtEnumEntry =>
            val enumEntryDesc = BindingUtils.getClassDescriptor(c.bindingContext(), e)

            val rcv = genThisFromContext(desc.toJsEnumCompanionType)
            val ctorIdent = genEnumCtorIdent(desc)
            val args = Nil ++ List(StringLiteral(e.getName), IntLiteral(enumCardinality))
            val newCall = New(enumEntryDesc.toJsClassType, ctorIdent, args)
            val select = Select(rcv, enumEntryDesc.toJsIdent)(desc.toJsClassType)

            enumCardinality += 1
            // this.ENTRY = new Enum$ENTRY(...)
            List(Assign(select, newCall))

          case _ => Nil

        }
        .flatten
        .toList

      val o = ClassType("O")
      val superCall = ApplyStatically(genThisFromContext(o), o, Ident("init___"), Nil)(NoType)

      val statements = Block(superCall :: initializations ++ List(valuesArray))

      val ctorIdent = Ident("init___")

      MethodDef(static = false, ctorIdent, Nil, NoType, Some(statements))(OptimizerHints.empty,
                                                                          None)
    }

    /*
     * public static MyEnum valueOf(String string) {
     *   if (string == entry1.toString)
     *    entry1
     *   else
     *     if (string == entry2.toString)
     *      entry2
     *     else
     *      ...
     * }
    */
    private def _genValueOfFunction: MemberDef = {
      val valueOfFunBody = {
        val entries = clsOrObj.getDeclarations.asScala.filter(_.isInstanceOf[KtEnumEntry]).map(_.asInstanceOf[KtEnumEntry])

        val lhs = VarRef(Ident(valueOfParamName))(ClassType("T"))

        val exceptionMsg = BinaryOp(BinaryOp.String_+, StringLiteral(s"Illegal enum constant ${desc.toJsName}."), lhs)
        val exception: Tree = Throw(New(ClassType("jl_IllegalArgumentException"), Ident("init___T"), List(exceptionMsg)))

        entries.foldLeft(exception){ (acc, entry) =>
          val entryDesc = BindingUtils.getClassDescriptor(c.bindingContext(), entry)

          val rhs = StringLiteral(entry.getName)
          val ret = Select(genThisFromContext(desc.toJsEnumCompanionType, desc), entryDesc.toJsIdent)(desc.toJsClassType)

          val structEq = Apply(lhs, Ident("equals__O__Z"), List(rhs))(BooleanType)

          If(structEq, ret, acc)(desc.toJsClassType)
        }
      }

      val args = List(ParamDef(Ident("string"), ClassType("T"), mutable = false, rest = false))
      val valueOfFun =
        MethodDef(static = false,
          Ident(s"${valueOfName}__T__$enumName"),
          args,
          desc.toJsClassType,
          Option(valueOfFunBody))(OptimizerHints.empty, None)

      valueOfFun
    }

    /*
     * val $VALUES: LEnumClass[]
     * def values_ALEnumClass(): LEnumClass[] = {
     *  this.$VALUES.clone__O().asInstanceOf[ALEnumClass]
     * }
     */
    private def _genValuesFunction: List[MemberDef] = {

      val valuesField = FieldDef(static = false, Ident(arrayValuesName), arrayTpe, mutable = false)

      val valuesFunBody = {
        AsInstanceOf(Apply(
          Select(
            genThisFromContext(desc.toJsEnumCompanionType, desc),
            Ident(arrayValuesName)
          )(arrayTpe),
          Ident(s"clone__O"),
          Nil
        )(AnyType), arrayTpe.arrayTypeRef)
      }

      val valuesFun =
        MethodDef(static = false,
          Ident("values__" + toInternal(arrayTpe)),
          Nil,
          valuesField.ftpe,
          Option(valuesFunBody))(OptimizerHints.empty, None)

      List(valuesField, valuesFun)

    }

    private[utils] def _memberDefinitions(declarations: List[KtDeclaration],
                                          onlyEnumEntries: Boolean = false): List[MemberDef] = {

      def _prop(decl: KtDeclaration) = {
        if (onlyEnumEntries) decl.isInstanceOf[KtEnumEntry]
        else !decl.isInstanceOf[KtEnumEntry]
      }

      declarations
        .filter(_prop)
        .collect {
          case p: KtProperty =>
            if (desc.isInterface)
              GenProperty(p).withAbstractGetterAndSetter
            else
              GenProperty(p).withGetterAndSetter

          case e: KtEnumEntry =>
            GenEnumEntry(e).withGetter

          case _: KtClassInitializer | _: KtSecondaryConstructor | _: KtClassOrObject =>
            Nil

          case declaration =>
            val decl = if (desc.isInterface) GenDeclaration(declaration).withAbstractBodies else GenDeclaration(declaration).tree

            List(decl)

        }
        .flatten
    }

    /**
      * Retrieves the list of all inherited declarations defined in interfaces
      *
      * @param cd A list of ClassDescriptor (the super interfaces)
      * @param acc A set containing all already added declarations (only keep first override)
      * @return A list of declarations
      */
    private def _inheritedDeclarations(cd: List[ClassDescriptor],
                                       acc: Set[String] = Set()): List[KtDeclaration] = cd.flatMap {
      i =>
        {
          val file = DescriptorToSourceUtils.getContainingFile(i)

          // FIXME: File can be null sometimes... Comparable for instance has no source file
          if (file == null)
            return Nil

          val declarations = new SJSIRLower().lower(file)

          val res = declarations.collect {
            case d: KtClassOrObject =>
              val desc = BindingUtils.getClassDescriptor(c.bindingContext(), d)

              if (desc.toJsClassIdent == i.toJsClassIdent)
                d.getDeclarations.asScala.filter {
                  case p: KtProperty =>
                    val pDesc = getPropertyDescriptor(c.bindingContext(), p)
                    !acc.contains(pDesc.toJsName)

                  case nf: KtNamedFunction =>
                    val nfDesc = getFunctionDescriptor(c.bindingContext(), nf)
                    !acc.contains(nfDesc.toJsBridgeIdent.name)

                  case _: KtClassInitializer | _: KtSecondaryConstructor | _: KtClassOrObject =>
                    false
                } else
                Seq()

          }.flatten

          res ++ _inheritedDeclarations(
            DescriptorUtilsKt.getSuperInterfaces(i).asScala.toList,
            acc ++ res.map {
              case p: KtProperty =>
                getPropertyDescriptor(c.bindingContext(), p).toJsName

              case nf: KtNamedFunction =>
                val nfDesc = getFunctionDescriptor(c.bindingContext(), nf)
                nfDesc.toJsBridgeIdent.name

            }
          )
        }
    }

    /**
      * Transforms a MethodDef of an interface into a MethodDef calling the default implementation
      * of the interface.
      * @param m The MethodDef to be transformed to a bridge
      * @param e The KtProperty or KtNamedFunction element, the bridge should be generated from
      * @return The transformed MethodDef
      */
    private def _toBridge(m: MemberDef, e: KtElement): MethodDef =  m match {
      case md @ MethodDef(_, name: Ident, argsParamDef, resultType, _) =>
        val elemDesc = e match {
          case p: KtProperty =>
            val pDesc = BindingUtils.getPropertyDescriptor(c.bindingContext(), p)
            if (name.originalName.toString.contains("get"))
              pDesc.getGetter
            else
              pDesc.getSetter
          case nf: KtNamedFunction => BindingUtils.getFunctionDescriptor(c.bindingContext(), nf)
        }
        val clsDesc = DescriptorUtils.getContainingClass(elemDesc)

        val newName = Ident(name.encodedName.replaceFirst("__" + clsDesc.toJsClassName, ""))

        val newBody = {
          val methodIdent = elemDesc.toJsMethodDeclIdent
          val defaultImplCls = clsDesc.toJsDefaultImplType
          val cls = clsDesc.toJsClassType
          val self = genThisFromContext(cls)
          val args = self :: argsParamDef.tail.map(_.ref)
          ApplyStatic(defaultImplCls, methodIdent, args)(md.resultType)
        }

        MethodDef(static = false, newName, argsParamDef.tail, resultType, Option(newBody))(
          OptimizerHints.empty,
          None)
    }

  }
}
