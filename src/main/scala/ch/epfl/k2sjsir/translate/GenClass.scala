package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.ClassDescriptor
import org.jetbrains.kotlin.descriptors.ClassKind.OBJECT
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.utils.BindingUtils._
import org.jetbrains.kotlin.psi.{KtClassOrObject, KtProperty}
import org.jetbrains.kotlin.resolve.descriptorUtil.DescriptorUtilsKt._
import org.scalajs.core.ir.ClassKind
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.{AnyType, NoType}

import scala.collection.JavaConverters._
import scala.collection.immutable.{List, Nil}

case class GenClass(d: KtClassOrObject)(implicit val c: TranslationContext) extends Gen[KtClassOrObject] {

  private val desc = getClassDescriptor(c.bindingContext(), d)
  private val optimizerHints = OptimizerHints.empty

  override def tree: ClassDef = {
    val idt = desc.toJsClassIdent
    val kind = if (isModule(desc)) ClassKind.ModuleClass else ClassKind.Class
    val superClass = getSuperClassOrAny(desc).toJsClassIdent
    val interfaces = getSuperInterfaces(desc).asScala.map(_.toJsClassIdent)
    val jsNativeLoadSpec = None

    val defs : List[Tree] = d.getDeclarations.asScala.collect {
      case p : KtProperty => GenProperty(p).withGetterAndSetter
      case decl => Seq(GenDeclaration(decl).tree)
    }.flatten.toList

    val hasMain = defs.exists {
      case MethodDef(_, Ident("main__V", _), _, _, _) => true
      case _ => false
    }
    val constructor = genConstructor
    val allDefs = defs.toList ++ (if (hasMain) manualExports(desc) else Nil) :+ constructor
    ClassDef(idt, kind, Some(superClass), interfaces.toList, jsNativeLoadSpec, allDefs)(optimizerHints)
  }

  private def manualExports(cd: ClassDescriptor): List[Tree] = {
    val receiver = This()(cd.toJsClassType)
    val body = Block(Apply(receiver, Ident("main__V", Some("main")), Nil)(NoType), Undefined())
    val main = MethodDef(static = false, StringLiteral("main"), Nil, AnyType, Some(body))(OptimizerHints.empty, None)
    val name = cd.getName.asString()
    val mod = if (isModule(cd)) ModuleExportDef(name) else Skip()
    List(main, mod)
  }

  private def isModule(c: ClassDescriptor): Boolean = c.getKind == OBJECT

  private def genConstructor : Tree = {
    val propsInit = d.getDeclarations.asScala.collect {
      case p: KtProperty =>
        val expr = GenExpr(p.getDelegateExpressionOrInitializer).tree
        Assign(Select(This()(desc.toJsClassType), Ident(p.getName))(expr.tpe), expr)

    }.toList
    MethodDef(false, Ident("init___"), List(), NoType, Some(Block(propsInit)))(optimizerHints, None)
  }
}
