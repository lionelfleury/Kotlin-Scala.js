package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.NameEncoder
import org.jetbrains.kotlin.descriptors.ClassDescriptor
import org.jetbrains.kotlin.ir.declarations.IrClass
import org.jetbrains.kotlin.ir.declarations.IrDeclarationKind._
import org.jetbrains.kotlin.name.SpecialNames.NO_NAME_PROVIDED
import org.jetbrains.kotlin.resolve.descriptorUtil.DescriptorUtilsKt
import org.jetbrains.kotlin.types.ErrorUtils
import org.scalajs.core.ir.Trees.{ClassDef, OptimizerHints}
import org.scalajs.core.ir.{ClassKind, Trees => js}

import scala.collection.JavaConversions._
import scala.language.implicitConversions

case class GenClass(irClass: IrClass, p: Positioner)
  extends AbstractGenerator(irClass, p) {

  private val dummyClass =
    js.ClassDef(js.Ident("Dummy"), ClassKind.Class, None, Nil, None, Nil)(OptimizerHints.empty)

  override def generate(): ClassDef = {
    val descriptor = irClass.getDescriptor

    if (ErrorUtils.isError(descriptor) || descriptor.getName == NO_NAME_PROVIDED) {
      throw new Error("Class descriptor error")
    }

    val classIdent = getClassName(descriptor)
    val kind = irClass.getDeclarationKind match {
      case CLASS => ClassKind.Class
      case MODULE => ClassKind.ModuleClass
      //      case ENUM_ENTRY =>
      case k => throw new Error(s"No match for ClassKind $k")
    }

    val superClassDescriptor = DescriptorUtilsKt.getSuperClassOrAny(descriptor)
    val superClass = getClassName(superClassDescriptor)

    val interfacesDescriptors = DescriptorUtilsKt.getSuperInterfaces(descriptor)
    val interfaces = for (i <- interfacesDescriptors) yield getClassName(i)

    val jsNativeLoadSpec = None

    val optimizerHints = OptimizerHints.empty

    val defs = for (d <- irClass.getDeclarations) yield GenDeclaration(d, p).generate()

    js.ClassDef(classIdent, kind, Some(superClass), interfaces.toList, jsNativeLoadSpec, defs.toList)(optimizerHints)
  }

  private def getClassName(d: ClassDescriptor): js.Ident = {
    val name = d.getName.asString()
    val isModule = d.isCompanionObject
    name match {
      case "Any" => js.Ident("O", Some("java.lang.Object"))
      case _ => NameEncoder.encodeClassFullNameIdent(name, isModule)
    }
  }

}
