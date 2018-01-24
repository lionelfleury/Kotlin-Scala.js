package ch.epfl.k2sjsir.translate

import org.jetbrains.kotlin.descriptors.ClassDescriptor
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.utils.BindingUtils
import org.jetbrains.kotlin.psi.KtEnumEntry
import org.scalajs.core.ir.Trees._
import ch.epfl.k2sjsir.utils.Utils._
import org.scalajs.core.ir.Position

case class GenEnumEntry(d: KtEnumEntry)(implicit val c: TranslationContext) extends IRNodeGen[KtEnumEntry, FieldDef] {

  private val desc = BindingUtils.getClassDescriptor(c.bindingContext(), d)
  private val entryIdent = desc.toJsIdent
  private val containingDesc = desc.getContainingDeclaration  match {
    case cd: ClassDescriptor => cd
    case x => throw new Exception(s"Unexpected descriptor $x")
  }
  private val containingTpe = containingDesc.toJsClassType

  override def tree: FieldDef = FieldDef(static = false, entryIdent, containingTpe, mutable = false)

  def withGetter: List[MemberDef] = {
    List(tree) ++ List(genGetter)
  }

  private def genGetter: MethodDef = {
    val static = false
    val resultTpe = containingTpe
    val getterIdent = Ident(d.getName + "__" + containingTpe.className)
    val args = Nil

    val body = genSelect

    MethodDef(static, getterIdent, args, resultTpe, Option(body))(OptimizerHints.empty, None)
  }

  private def genSelect(implicit pos: Position): Select = {
    val rcv = genThisFromContext(containingDesc.toJsEnumCompanionType, desc)

    Select(rcv, entryIdent)(containingTpe)
  }
}
