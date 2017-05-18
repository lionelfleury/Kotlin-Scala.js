package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.{PropertyGetterDescriptor, PropertySetterDescriptor}
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.utils.BindingUtils
import org.jetbrains.kotlin.psi.KtProperty
import org.jetbrains.kotlin.resolve.DescriptorUtils
import org.jetbrains.kotlin.resolve.DescriptorUtils._
import org.scalajs.core.ir.Position
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.{AnyType, NoType}

case class GenProperty(d: KtProperty)(implicit val c: TranslationContext) extends Gen[KtProperty] {

  import GenProperty._

  val desc = BindingUtils.getPropertyDescriptor(c.bindingContext(), d)

  override def tree: Tree = {
    val idt = desc.toJsIdent
    val static = DescriptorUtils.isStaticDeclaration(desc)
    FieldDef(static, idt, desc.getType.toJsType, desc.isVar)
  }

  def withGetterAndSetter: Seq[Tree] = {
    val t = List(tree)
    val g  = Option(desc.getGetter).map(get => getter(get))
    val s =  Option(desc.getSetter).map(set => setter(set))
    t ++ g.toList ++ s.toList
  }

}

object GenProperty {
  private[translate] def getter(f: PropertyGetterDescriptor)(implicit pos: Position): Tree = {
    val property = f.getCorrespondingProperty
    val rtpe = property.getReturnType.toJsType
    val tpe = getClassDescriptorForType(property.getDispatchReceiverParameter.getType).toJsClassType

    val body = Some(Select(This()(tpe), property.toJsIdent)(rtpe))
    MethodDef(static = false, f.toJsMethodIdent, List(), rtpe, body)(OptimizerHints.empty, None)
  }

  private[translate] def setter(f: PropertySetterDescriptor)(implicit pos: Position): Tree = {
    val property = f.getCorrespondingProperty

    val tpe = getClassDescriptorForType(property.getDispatchReceiverParameter.getType).toJsClassType
    val rtpe = property.getReturnType.toJsType

    val params = List(ParamDef(Ident("set"), rtpe, mutable = false, rest = false))
    val body = Some(Assign(Select(This()(tpe), property.toJsIdent)(rtpe), VarRef(Ident("set"))(rtpe)))
    MethodDef(static = false, f.toJsMethodIdent, params, rtpe, body)(OptimizerHints.empty, None)
  }

}
