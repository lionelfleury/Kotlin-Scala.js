package ch.epfl.k2sjsir.translate

import ch.epfl.k2sjsir.utils.Utils
import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.js.translate.utils.BindingUtils
import org.jetbrains.kotlin.psi.KtArrayAccessExpression
import org.jetbrains.kotlin.types.KotlinType
import org.scalajs.core.ir.Trees.{ArraySelect, Tree}
import org.scalajs.core.ir.Types._

import scala.collection.JavaConverters._

case class GenArrayAccess(d: KtArrayAccessExpression)(implicit val c: TranslationContext) extends Gen[KtArrayAccessExpression] {

  override def tree: Tree = {
    assert(d.getIndexExpressions.size() == 1)
    val index = d.getIndexExpressions.asScala.map(a => GenExpr(a).tree).toList
    val array = GenExpr(d.getArrayExpression).tree
    val t = BindingUtils.getTypeForExpression(c.bindingContext(), d.getArrayExpression)

    val tpe = if (t.getArguments.isEmpty) getArrayElementsType(t) else t.getArguments.get(0).getType.toJsType

    ArraySelect(array, index.head)(tpe)
  }

  def getArrayElementsType(tpe: KotlinType): Type = {
    Map(
      "kotlin.IntArray" -> IntType,
      "kotlin.BooleanArray" -> BooleanType,
      "kotlin.CharArray" -> CharType,
      "kotlin.ByteArray" -> ByteType,
      "kotlin.ShortArray" -> ShortType,
      "kotlin.FloatArray" -> FloatType,
      "kotlin.LongArray" -> LongType,
      "kotlin.DoubleArray"-> DoubleType
    ).getOrElse(Utils.getName(tpe), throw new Exception(s"Unhandled $tpe"))
  }

}
