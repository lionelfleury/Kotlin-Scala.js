package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.NameEncoder._
import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.ir.expressions.IrGetField
import org.jetbrains.kotlin.resolve.DescriptorUtils._
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.ClassType

case class GenGetField(d: IrGetField, p: Positioner) extends Gen[IrGetField] {

  def tree: Tree = {
    val pd = d.getDescriptor
    val idt = pd.toJsIdent
    val tpe = pd.getReturnType.toJsType
    val static = isStaticDeclaration(pd)
    if (static) {
      val className = getFqName(pd.getContainingDeclaration).asString()
      val c = ClassType(encodeClassName(className, "$"))
      if (className.startsWith("java")) { //TODO: Check for compatibility with Scala.js
        Apply(LoadModule(c), idt, Nil)(tpe)
      } else SelectStatic(c, idt)(tpe)
    }
    else Select(GenExpr(d.getReceiver, p).tree, idt)(tpe)
  }

}
