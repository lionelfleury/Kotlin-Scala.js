package ch.epfl.k2sjsir.lower

import org.jetbrains.kotlin.ir.declarations.impl.IrClassImpl
import org.jetbrains.kotlin.ir.declarations.{IrClass, IrDeclaration, IrFile}

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

class CompanionLowering(f: IrFile) {

  private val classes = ListBuffer[IrClass]()

  def lower(): Unit = {
    f.getDeclarations.asScala.foreach {
      case irClass: IrClass =>
        classes += irClass
        traverse(irClass)
    }
    f.getDeclarations.clear()
    f.getDeclarations.addAll(classes.asJava)
  }

  private def traverse(irClass: IrClass): Unit = {
    val ds = irClass.getDeclarations.asScala
    val buffer = ListBuffer[IrDeclaration]()
    for (i <- ds.indices; c = ds(i)) c match {
      case c: IrClass =>
        classes += c
        traverse(c)
      case other => buffer += other
    }
    irClass.getDeclarations.clear()
    irClass.getDeclarations.addAll(buffer.asJava)
  }

}
