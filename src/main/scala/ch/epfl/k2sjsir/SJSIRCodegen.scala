package ch.epfl.k2sjsir

import java.io.{File, FileOutputStream}

import ch.epfl.k2sjsir.codegen.{GenClass, Positioner}
import ch.epfl.k2sjsir.utils.Utils._
import org.jetbrains.kotlin.descriptors.ClassDescriptor
import org.jetbrains.kotlin.ir.declarations.IrClass
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.{InfoSerializers, Infos, InvalidIRException, Serializers}

class SJSIRCodegen(outDir: String) {

  def generate(d: IrClass, p: Positioner): Unit = {
    if (outDir == null) sys.error("No output directory found...")
    val tree: ClassDef = GenClass(d, p).tree
    genIRFile(outDir, d.getDescriptor, tree)
  }

  private def genIRFile(outDir: String, cd: ClassDescriptor, tree: ClassDef): Unit = {
    val name = cd.toJsClassName.drop(1).replace("_", "/")
    val file = new File(outDir, name + ".sjsir")
    file.getParentFile.mkdir()
    val output = new FileOutputStream(file)
    try {
      InfoSerializers.serialize(output, Infos.generateClassInfo(tree))
      Serializers.serialize(output, tree)
    } catch {
      case e: InvalidIRException => e.tree match {
        case _: UndefinedParam => println("Found a dangling UndefinedParam at " +
          s"${e.tree.pos}. This is likely due to a bad interaction " +
          "between a macro or a compiler plugin and the Scala.js " +
          "compiler plugin. If you hit this, please let us know.")
        case _ => println("The Scala.js compiler generated " +
          s"invalid IR for this class. Please report this as a bug. IR: ${e.tree}")
      }
    } finally {
      output.close()
    }
  }

}
