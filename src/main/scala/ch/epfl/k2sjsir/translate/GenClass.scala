package ch.epfl.k2sjsir.translate

import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.psi.KtClassOrObject
import org.scalajs.core.ir.Trees._
import org.jetbrains.kotlin.js.translate.utils.BindingUtils.getClassDescriptor
import scala.collection.JavaConverters._

case class GenClass(d: KtClassOrObject, c: TranslationContext) extends Gen[KtClassOrObject] {

  override def tree: Tree = {
    val desc = getClassDescriptor(c.bindingContext(), d)
    for (ct <- desc.getConstructors.asScala) println(ct.getName.asString())
    Skip()
  }

}
