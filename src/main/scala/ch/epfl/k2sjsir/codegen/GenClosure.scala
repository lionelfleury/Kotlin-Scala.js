package ch.epfl.k2sjsir.codegen

import ch.epfl.k2sjsir.utils.NameEncoder
import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.ir.expressions.IrCallableReference
import org.jetbrains.kotlin.resolve.DescriptorUtils
import org.scalajs.core.ir.Trees
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.ClassType

import scala.collection.JavaConverters._


case class GenClosure(d: IrCallableReference, p: Positioner) extends Gen[IrCallableReference] {

  override def tree: Tree = {
    val desc = d.getDescriptor
    val containingClass = Option(DescriptorUtils.getContainingClass(desc))
    val ct = containingClass.fold(ClassType(NameEncoder.encodeWithSourceFile(desc)))(cc => cc.toJsClassType)
    val static = DescriptorUtils.isStaticDeclaration(desc)

    val methodName = desc.toJsMethodIdent
    val parameters = desc.getValueParameters.asScala.map(x => VarRef(Ident(x.getName.toString))(x.getType.toJsClassType)).toList
    val body = if (static)
      ApplyStatic(ct, methodName, parameters)(desc.getReturnType.toJsType)
    else
      Apply(VarRef(Ident("$this"))(ct), methodName, parameters)(desc.getReturnType.toJsClassType)

    val closureParams = desc.getValueParameters.asScala.map(_.toJsParamDef).toList
    val closure = containingClass match {
      case Some(cc) =>
        val captureParams = List(ParamDef(Ident("$this"), ct, mutable = false, rest = false))
        val captureValues = List[Trees.Tree](This()(ct))
        val closureParams = desc.getValueParameters.asScala.map(_.toJsParamDef).toList
        Closure(captureParams, closureParams, body, captureValues)
      case None => Closure(List(), closureParams, body, List())
    }

    New(ClassType(s"sjsr_AnonFunction${closureParams.size}"), Ident(s"init___sjs_js_Function${closureParams.size}"), List(closure))
  }

}
