package ch.epfl.k2sjsir

import java.io.{File, FileOutputStream}

import ch.epfl.k2sjsir.codegen.Positioner
import org.jetbrains.kotlin.backend.jvm.JvmBackendContext
import org.jetbrains.kotlin.config.JVMConfigurationKeys
import org.jetbrains.kotlin.descriptors.ClassDescriptor
import org.jetbrains.kotlin.ir.declarations.{IrClass, IrDeclarationKind}
import org.scalajs.core.ir.Trees.OptimizerHints
import org.scalajs.core.ir.{ClassKind, InfoSerializers, Infos, InvalidIRException, Serializers, Trees => js}

class Codegen(val context: JvmBackendContext, val positioner: Positioner) {

  def generateClass(irClass: IrClass): Unit = {
    import CodeGen._

    // Call here to create the tree...
    implicit val pos = positioner.getPos(irClass.getStartOffset)
    val tree = js.ClassDef(js.Ident("Test"), ClassKind.Class, None, Nil, None, Nil)(OptimizerHints.empty)

    val outDir = context.getState.getConfiguration.get(JVMConfigurationKeys.OUTPUT_DIRECTORY).toString
    if (outDir == null) sys.error("No output directory found...")
    val suffix = if (irClass.getDeclarationKind == IrDeclarationKind.MODULE) "$" else ""
    genIRFile(outDir, irClass.getDescriptor, suffix, tree)
  }

}

private object CodeGen {

  def genIRFile(outDir: String, sym: ClassDescriptor, suffix: String, tree: js.ClassDef): Unit = {
    val file = new File(outDir + "/" + sym.getName + suffix + ".sjsir")
    file.getParentFile.mkdir()
    val output = new FileOutputStream(file)
    try {
      InfoSerializers.serialize(output, Infos.generateClassInfo(tree))
      Serializers.serialize(output, tree)
    } catch {
      case e: InvalidIRException => e.tree match {
        case _: js.UndefinedParam => println("Found a dangling UndefinedParam at " +
          "${e.tree().pos()}. This is likely due to a bad interaction " +
          "between a macro or a compiler plugin and the Scala.js " +
          "compiler plugin. If you hit this, please let us know.")
        case _ => println("The Scala.js compiler generated " +
          "invalid IR for this class. Please report this as a bug. IR: " +
          e.tree)
      }
    } finally {
      output.close()
    }
  }

}