package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.ClassKind.{CLASS, OBJECT}
import org.jetbrains.kotlin.ir.declarations.IrClass
import org.jetbrains.kotlin.name.SpecialNames.NO_NAME_PROVIDED
import org.jetbrains.kotlin.resolve.descriptorUtil.DescriptorUtilsKt.{getSuperClassOrAny, getSuperInterfaces}
import org.jetbrains.kotlin.types.ErrorUtils
import org.scalajs.core.ir.ClassKind
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.{AnyType, NoType}

import scala.collection.JavaConverters._

case class GenClass(d: IrClass, p: Positioner) extends Gen[IrClass] {
  private val cd = d.getDescriptor

  override def tree: ClassDef = {
    if (ErrorUtils.isError(cd) || cd.getName == NO_NAME_PROVIDED)
      throw new Error(s"Class descriptor error: $cd")
    val idt = cd.toJsClassIdent
    val kind = cd.getKind match {
      case CLASS => ClassKind.Class
      case OBJECT => ClassKind.ModuleClass
      case k => throw new Error(s"No match for ClassKind: $k")
    }
    val superClass = getSuperClassOrAny(cd).toJsClassIdent
    val interfaces = getSuperInterfaces(cd).asScala.map(_.toJsClassIdent)
    // TODO: Voir ce qu'on peut charger ici...
    val jsNativeLoadSpec = None
    val optimizerHints = OptimizerHints.empty
    val defs = for (d <- d.getDeclarations.asScala) yield GenDeclaration(d, p).tree
    val allDefs = defs ++ manualExportMain
    ClassDef(idt, kind, Some(superClass), interfaces.toList, jsNativeLoadSpec, allDefs.toList)(optimizerHints)
  }

  private def manualExportMain: List[Tree] = {
    val receiver = This()(cd.toJsClassType)
    val body = Block(Apply(receiver, Ident("main__V", Some("main")), Nil)(NoType), Undefined())
    val main = MethodDef(static = false, StringLiteral("main"), Nil, AnyType, Some(body))(OptimizerHints.empty, None)
    List(main, ModuleExportDef(cd.getName.asString()))
  }

}
