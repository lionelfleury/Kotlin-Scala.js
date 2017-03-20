package ch.epfl.k2sjsir.lower

import org.jetbrains.kotlin.backend.common.ClassLoweringPass
import org.jetbrains.kotlin.backend.jvm.JvmBackendContext
import org.jetbrains.kotlin.ir.IrStatement
import org.jetbrains.kotlin.ir.declarations._
import org.jetbrains.kotlin.ir.expressions.IrStatementContainer
import org.jetbrains.kotlin.ir.expressions.impl.{IrBlockBodyImpl, IrGetValueImpl, IrSetFieldImpl}

import scala.collection.JavaConversions._
import scala.language.implicitConversions

class FieldLowering(val context: JvmBackendContext) extends ClassLoweringPass {

  private val emptyStatList = List[IrStatement]()

  override def lower(irClass: IrClass) {
    val ds = irClass.getDeclarations.toList
    val inits = ds.collect {
      case d: IrField =>
        val f = Option(d.getInitializer).map(_.getExpression)
        if (f.nonEmpty) {
          val (start, end) = f.map(f => (f.getStartOffset, f.getEndOffset)).get
          val receiver =
            if (d.getDescriptor.getDispatchReceiverParameter != null)
              new IrGetValueImpl(start, end, irClass.getDescriptor.getThisAsReceiverParameter, null)
            else null
          List(new IrSetFieldImpl(start, end, d.getDescriptor, receiver, f.get, null, null))
        } else emptyStatList
      case declaration: IrAnonymousInitializer =>
        declaration.getBody.getStatements.toList
    }
    val const = ds.collect { case d: IrConstructor if d.getDescriptor.isPrimary => d }.head
    val stats = const.getBody match {
      case b: IrStatementContainer => b.getStatements.toList
      case _ => emptyStatList
    }
    val allStats = stats ++ inits.flatten
    val newBody = new IrBlockBodyImpl(irClass.getStartOffset, irClass.getEndOffset, allStats)
    const.setBody(newBody)
  }

}
