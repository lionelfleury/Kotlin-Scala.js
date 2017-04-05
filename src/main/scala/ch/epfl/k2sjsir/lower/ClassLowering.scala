package ch.epfl.k2sjsir.lower

import org.jetbrains.kotlin.ir.declarations.{IrClass, IrDeclaration, IrFile}

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

class ClassLowering {

  private val classes = ListBuffer[IrClass]()

  def lower(f: IrFile): Unit = {
    f.getDeclarations.asScala.foreach {
      case c: IrClass =>
        val ds = traverse(c.getDeclarations.asScala, Seq())
        addRemove(c, ds)
      case _ => sys.error("Should be IrClass!")
    }
    f.getDeclarations.clear()
    f.getDeclarations.addAll(classes.asJava)
    classes.clear()
  }

  private def traverse(decls: Seq[IrDeclaration],
                       acc: Seq[IrDeclaration]): Seq[IrDeclaration] = decls match {
    case (c: IrClass) +: t =>
      val ds = traverse(c.getDeclarations.asScala, Seq())
      addRemove(c, ds)
      traverse(t, acc)
    case h +: t => traverse(t, h +: acc)
    case _ => acc
  }

  private def addRemove(i: IrClass, ds: Seq[IrDeclaration]): Unit = {
    i.getDeclarations.clear()
    i.getDeclarations.addAll(ds.asJava)
    classes += i
  }

}