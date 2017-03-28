package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.ClassKind
import org.jetbrains.kotlin.ir.IrStatement
import org.jetbrains.kotlin.ir.declarations._
import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.NoType

import scala.collection.JavaConverters._

case class GenStat(d: IrStatement, p: Positioner) extends Gen[IrStatement] {

  def tree: Tree = d match {
    case c: IrDelegatingConstructorCall =>
      val cd = c.getDescriptor
      val tpe = cd.getConstructedClass.toJsClassType
      val args = cd.getValueParameters.asScala.map(_.toJsVarRef)
      ApplyStatically(This()(tpe), tpe, cd.toJsMethodIdent, args.toList)(NoType)
    case c: IrInstanceInitializerCall =>
      val tpe = c.getClassDescriptor.toJsClassType
      if (c.getClassDescriptor.getKind == ClassKind.OBJECT) {
        val tpe = c.getClassDescriptor.toJsClassType
        StoreModule(tpe, This()(tpe))
      }
      else Skip()
    case b: IrBlock => GenBlock(b, p).tree
    case f: IrFunction => GenFun(f, p).tree
    case r: IrReturn => GenExpr(r.getValue, p).tree
    case s: IrSetField => GenSetField(s, p).tree
    case i: IrWhen => GenWhen(i, p).tree
    case v: IrVariable => GenVar(v, p).tree
    case c: IrCall => GenCall(c, p).tree
    case w: IrWhileLoop => GenWhile(w, p).tree
    case w: IrDoWhileLoop => GenDoWhile(w, p).tree
    case oc: IrTypeOperatorCall => GenExpr(oc.getArgument, p).tree
    case _ => notImplemented
  }

}
