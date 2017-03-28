package ch.epfl.k2sjsir

/*
 * Copyright 2010-2015 JetBrains s.r.o.
 * Adapted 2017 by Lionel Fleury and Guillaume Tournigand
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import ch.epfl.k2sjsir.codegen.Positioner
import ch.epfl.k2sjsir.lower.SJSIRLower
import org.jetbrains.kotlin.backend.jvm.{JvmBackendContext, JvmLower}
import org.jetbrains.kotlin.codegen.CompilationErrorHandler
import org.jetbrains.kotlin.codegen.state.GenerationState
import org.jetbrains.kotlin.config.JVMConfigurationKeys.OUTPUT_DIRECTORY
import org.jetbrains.kotlin.ir.declarations.{IrClass, IrFile}
import org.jetbrains.kotlin.psi2ir.Psi2IrTranslator

import scala.collection.JavaConverters._

object Backend {

  def doGenerateFiles(state: GenerationState): Unit = {
    // TODO multifile classes support
    val psi2ir = new Psi2IrTranslator()
    if (state == null) return
    val psi2irContext = psi2ir.createGeneratorContext(state.getModule, state.getBindingContext)
    val irModuleFragment = psi2ir.generateModuleFragment(psi2irContext, state.getFiles)
    val jvmBackendContext = new JvmBackendContext(state, psi2irContext.getSourceManager, psi2irContext.getIrBuiltIns)
    irModuleFragment.getFiles.asScala.foreach { irFile =>
      try {
        generateFile(irFile, jvmBackendContext)
        state.afterIndependentPart()
      } catch {
        case e: Throwable =>
          CompilationErrorHandler.THROW_EXCEPTION.reportException(e, null) // TODO ktFile.virtualFile.url
      }
    }
  }

  private def generateFile(irFile: IrFile, context: JvmBackendContext): Unit = {
    val lower = new SJSIRLower(context)
    val p = new Positioner(irFile)
    val outDir = context.getState.getConfiguration.get(OUTPUT_DIRECTORY).toString
    val codegen = new SJSIRCodegen(outDir)
    lower.lower(irFile)
    irFile.getDeclarations.asScala.foreach {
      case i: IrClass => codegen.generate(i, p)
      case c => throw new AssertionError(s"Declaration should be IrClass, got: $c")
    }
  }

}
