package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.PropertyGetterDescriptor
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.utils.{BindingUtils, JsDescriptorUtils}
import org.jetbrains.kotlin.psi.KtProperty
import org.jetbrains.kotlin.resolve.{BindingContext, DescriptorUtils}
import org.scalajs.core.ir.Trees._
import org.jetbrains.kotlin.js.translate.utils.TranslationUtils._
import org.jetbrains.kotlin.resolve.calls.callUtil.CallUtilKt

case class GenProperty(d: KtProperty)(implicit val c: TranslationContext) extends Gen[KtProperty] {

  override def tree: Tree = {
    val pd = BindingUtils.getPropertyDescriptor(c.bindingContext(), d)
    val idt = pd.toJsIdent
    val g  = Option(pd.getGetter).map(x => getter(x))

    val s = None //Option(pd.getSetter).map(setter)
    val static = DescriptorUtils.isStaticDeclaration(pd)
    PropertyDef(static, idt, g, s)
  }

  private def getter(f: PropertyGetterDescriptor): Tree = {
    val b = GenExpr(d.getDelegateExpressionOrInitializer).tree
    val a = f.getCorrespondingProperty
    //backingFieldReference(c, f)
    Block(VarRef(Ident("b"))(b.tpe))
  }


}
