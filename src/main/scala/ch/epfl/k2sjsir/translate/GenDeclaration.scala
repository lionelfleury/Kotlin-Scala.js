package ch.epfl.k2sjsir.translate

import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.psi.{KtDeclaration, KtNamedFunction, KtProperty}
import org.scalajs.core.ir.Trees._

case class GenDeclaration(d: KtDeclaration)(implicit val c: TranslationContext) extends IRNodeGen[KtDeclaration, MemberDef] {

  override def tree: MemberDef = d match {
    case f: KtNamedFunction => GenFun(f).tree
    case p: KtProperty => GenProperty(p).tree
    case _ =>
      notImplementedMemberDef("Default case on tree")
  }

  def withAbstractBodies: MemberDef = d match {
    case f: KtNamedFunction => GenFun(f).withAbstractBody
    case _: KtProperty => notImplementedMemberDef("Unsupported abstract property")
    case _ => notImplementedMemberDef("Default case on tree")
  }

}
