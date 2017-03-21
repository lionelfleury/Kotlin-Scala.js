package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.ir.IrStatement
import org.jetbrains.kotlin.ir.declarations._
import org.jetbrains.kotlin.ir.expressions._
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types

import scala.collection.JavaConverters._

case class GenStat(d: IrStatement, p: Positioner) extends Gen[IrStatement] {

  def tree: Tree = d match {
    case c: IrDelegatingConstructorCall =>
      val cd = c.getDescriptor
      val tpe = cd.getContainingDeclaration.toJsClassType
      val args = cd.getValueParameters.asScala.map(_.toJsVarRef)
      ApplyStatically(This()(Types.NoType), tpe, cd.toJsMethodIdent, args.toList)(Types.NoType)
    case c: IrInstanceInitializerCall =>
      StoreModule(c.getClassDescriptor.toJsClassType, This()(Types.NoType))
    case b: IrBlock => GenBlock(b, p).tree
    case f: IrFunction => GenFun(f, p).tree
    case r: IrReturn => GenReturn(r, p).tree
    case s: IrSetField => GenSetField(s, p).tree
    case i: IrWhen => GenWhen(i, p).tree
    case v: IrVariable => GenVar(v, p).tree
    case c: IrCall => GenCall(c, p).tree
    case _ => notImplemented
  }

}
