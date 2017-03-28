package ch.epfl.k2sjsir.lower

import org.jetbrains.kotlin.ir.declarations.{IrClass, IrFile}

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

class ClassLowering {

  private val classes = ListBuffer[IrClass]()

  def lower(f: IrFile): Unit = {
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
    for (i <- ds.indices; c = ds(i)) c match {
      case c: IrClass =>
        classes += c
        irClass.getDeclarations.remove(i)
        traverse(c)
      case _ =>
    }
  }

}
