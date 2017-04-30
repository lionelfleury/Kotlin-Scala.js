package ch.epfl.k2sjsir.translate

import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.psi.{KtDeclaration, KtNamedFunction, KtProperty}
import org.scalajs.core.ir.Trees._

case class GenDeclaration(d: KtDeclaration)(implicit val c: TranslationContext) extends Gen[KtDeclaration] {

  override def tree: Tree = d match {
    case f: KtNamedFunction => GenFun(f).tree
    case p: KtProperty => GenProperty(p).tree
    /*case b: IrBlock => GenBlock(b, p).tree
    case x: IrField => GenField(x, p).tree
    case x: IrProperty => GenProperty(x, p).tree
    //    case _: IrAnonymousInitializer => Skip()
    //    case _: IrTypeAlias => Skip()
    case c: IrClass => GenClass(c, p).tree*/
    case _ => notImplemented
  }

}
