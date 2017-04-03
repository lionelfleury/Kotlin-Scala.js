package ch.epfl.k2sjsir.lower

import org.jetbrains.kotlin.descriptors.ReceiverParameterDescriptor
import org.jetbrains.kotlin.ir.IrStatement
import org.jetbrains.kotlin.ir.declarations._
import org.jetbrains.kotlin.ir.expressions.IrStatementContainer
import org.jetbrains.kotlin.ir.expressions.impl.{IrBlockBodyImpl, IrGetValueImpl, IrSetFieldImpl}

import scala.collection.JavaConverters._

class ConstructorLowering {

  def lower(f: IrFile): Unit = f.getDeclarations.asScala.foreach {
    case irClass: IrClass =>
      val declarations = irClass.getDeclarations.asScala
      val const = getPrimaryConstructor(declarations)
      val stats = getStatements(const) ++ getInitializers(irClass, declarations)
      val newBody = new IrBlockBodyImpl(irClass.getStartOffset, irClass.getEndOffset, stats.asJava)
      const.foreach(_.setBody(newBody))
  }

  private def getPrimaryConstructor(ds: Seq[IrDeclaration]) = ds.collect {
    case d: IrConstructor if d.getDescriptor.isPrimary => d
  }.headOption

  private def getStatements(c: Option[IrConstructor]) = c.map(_.getBody).map {
    case l: IrStatementContainer => l.getStatements.asScala
  }.getOrElse(Nil)

  private def getInitializers(i: IrClass, ds: Seq[IrDeclaration]): Seq[IrStatement] = ds.collect {
    case d: IrField if d.getInitializer != null =>
      Option(d.getInitializer.getExpression).map { exp =>
        val (start, end) = (exp.getStartOffset, exp.getEndOffset)
        val receiver = if (hasReceiver(d)) new IrGetValueImpl(start, end, getThis(i), null) else null
        Seq(new IrSetFieldImpl(start, end, d.getDescriptor, receiver, exp, null, null))
      }.getOrElse(Nil)
    case d: IrAnonymousInitializer =>
      i.getDeclarations.remove(d)
      d.getBody.getStatements.asScala
  }.flatten

  private def hasReceiver(d: IrField): Boolean =
    d.getDescriptor.getDispatchReceiverParameter != null

  private def getThis(i: IrClass): ReceiverParameterDescriptor =
    i.getDescriptor.getThisAsReceiverParameter

}
