package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.ClassDescriptor
import org.jetbrains.kotlin.descriptors.ClassKind.OBJECT
import org.jetbrains.kotlin.ir.declarations.IrClass
import org.jetbrains.kotlin.name.SpecialNames.NO_NAME_PROVIDED
import org.jetbrains.kotlin.resolve.descriptorUtil.DescriptorUtilsKt.{getSuperClassOrAny, getSuperInterfaces}
import org.jetbrains.kotlin.types.ErrorUtils
import org.scalajs.core.ir.ClassKind
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.{AnyType, NoType}

import scala.collection.JavaConverters._
import scala.collection.immutable._

case class GenClass(d: IrClass, p: Positioner) extends Gen[IrClass] {
  private val cd = d.getDescriptor

  override def tree: ClassDef = {
    if (ErrorUtils.isError(cd) || cd.getName == NO_NAME_PROVIDED)
      throw new Error(s"Class descriptor error: $cd")
    val idt = cd.toJsClassIdent
    val kind = if (isModule(cd)) ClassKind.ModuleClass else ClassKind.Class
    val superClass = getSuperClassOrAny(cd).toJsClassIdent
    val interfaces = getSuperInterfaces(cd).asScala.map(_.toJsClassIdent)
    // TODO: Voir ce qu'on peut charger ici...
    val jsNativeLoadSpec = None
    val optimizerHints = OptimizerHints.empty
    val defs = for (d <- d.getDeclarations.asScala) yield GenDeclaration(d, p).tree
    val hasMain = defs.exists {
      case MethodDef(_, Ident("main__V", _), _, _, _) => true
      case _ => false
    }
    val allDefs = defs.toList ++ (if (hasMain) manualExports else Nil)
    ClassDef(idt, kind, Some(superClass), interfaces.toList, jsNativeLoadSpec, allDefs)(optimizerHints)
  }

  private def manualExports: List[Tree] = {
    val receiver = This()(cd.toJsClassType)
    val body = Block(Apply(receiver, Ident("main__V", Some("main")), Nil)(NoType), Undefined())
    val main = MethodDef(static = false, StringLiteral("main"), Nil, AnyType, Some(body))(OptimizerHints.empty, None)
    val name = cd.toJsClassName.drop(1).dropRight(1)
    val mod = if (isModule(cd)) ModuleExportDef(name) else Skip()
    List(main, mod)
  }

  private def isModule(c: ClassDescriptor): Boolean = c.getKind == OBJECT

}
