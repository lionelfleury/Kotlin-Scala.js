package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.NameEncoder
import org.jetbrains.kotlin.descriptors.ClassDescriptor
import org.jetbrains.kotlin.descriptors.ClassKind._
import org.jetbrains.kotlin.ir.declarations.IrClass
import org.jetbrains.kotlin.name.SpecialNames.NO_NAME_PROVIDED
import org.jetbrains.kotlin.resolve.descriptorUtil.DescriptorUtilsKt
import org.jetbrains.kotlin.types.ErrorUtils
import org.scalajs.core.ir.Trees.{ClassDef, OptimizerHints}
import org.scalajs.core.ir.{ClassKind, Position, Types, Trees => js}

import scala.collection.JavaConversions._
import scala.language.implicitConversions

case class GenClass(d: IrClass, p: Positioner) extends Gen[IrClass] {

  import GenClass._

  private val dummyClass =
    js.ClassDef(js.Ident("Dummy"), ClassKind.Class, None, Nil, None, Nil)(OptimizerHints.empty)

  override def tree: ClassDef = {
    val descriptor = d.getDescriptor

    if (ErrorUtils.isError(descriptor) || descriptor.getName == NO_NAME_PROVIDED) {
      throw new Error("Class descriptor error")
    }

    val classIdent = getClassIdent(descriptor, pos)
    val kind = descriptor.getKind match {
      case CLASS => ClassKind.Class
      case OBJECT => ClassKind.ModuleClass
      //      case ENUM_ENTRY =>
      case k => throw new Error(s"No match for ClassKind $k")
    }

    val superClassDescriptor = DescriptorUtilsKt.getSuperClassOrAny(descriptor)
    val superClass = getClassIdent(superClassDescriptor, pos)

    val interfacesDescriptors = DescriptorUtilsKt.getSuperInterfaces(descriptor)
    val interfaces = for (i <- interfacesDescriptors) yield getClassIdent(i, pos)

    val jsNativeLoadSpec = None

    val optimizerHints = OptimizerHints.empty

    val defs = for (d <- d.getDeclarations) yield GenDeclaration(d, p).tree

    js.ClassDef(classIdent, kind, Some(superClass), interfaces.toList, jsNativeLoadSpec, defs.toList)(optimizerHints)
  }

}

object GenClass {

  def getClassType(d: ClassDescriptor): Types.ClassType = {
    Types.ClassType(NameEncoder.encodeClassFullName(d))
  }

  def getClassIdent(d: ClassDescriptor, pos: Position): js.Ident = {
    NameEncoder.encodeClassFullNameIdent(d)(pos)
  }

}
