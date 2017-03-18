package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.Utils._
import org.jetbrains.kotlin.descriptors.ClassKind.{CLASS, OBJECT}
import org.jetbrains.kotlin.ir.declarations.IrClass
import org.jetbrains.kotlin.name.SpecialNames.NO_NAME_PROVIDED
import org.jetbrains.kotlin.resolve.descriptorUtil.DescriptorUtilsKt.{getSuperClassOrAny, getSuperInterfaces}
import org.jetbrains.kotlin.types.ErrorUtils
import org.scalajs.core.ir.ClassKind
import org.scalajs.core.ir.Trees.{ClassDef, OptimizerHints}

import scala.collection.JavaConversions._
import scala.language.implicitConversions

case class GenClass(d: IrClass, p: Positioner) extends Gen[IrClass] {

  override def tree: ClassDef = {
    val cd = d.getDescriptor
    if (ErrorUtils.isError(cd) || cd.getName == NO_NAME_PROVIDED)
      throw new Error(s"Class descriptor error: $cd")
    val idt = cd.toClassIdent
    val kind = cd.getKind match {
      case CLASS => ClassKind.Class
      case OBJECT => ClassKind.ModuleClass
      case k => throw new Error(s"No match for ClassKind: $k")
    }
    val sCidt = getSuperClassOrAny(cd).toClassIdent
    val interfaces = getSuperInterfaces(cd).map(_.toClassIdent)
    val jsNativeLoadSpec = None
    // TODO: Voir ce qu'on peut charger ici...
    val optimizerHints = OptimizerHints.empty
    val defs = for (d <- d.getDeclarations) yield GenDeclaration(d, p).tree
    ClassDef(idt, kind, Some(sCidt), interfaces.toList, jsNativeLoadSpec, defs.toList)(optimizerHints)
  }

}
