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
import org.jetbrains.kotlin.backend.jvm.{JvmBackendContext, JvmLower}
import org.jetbrains.kotlin.codegen.CompilationErrorHandler
import org.jetbrains.kotlin.codegen.state.GenerationState
import org.jetbrains.kotlin.ir.declarations.{IrClass, IrFile}
import org.jetbrains.kotlin.psi2ir.Psi2IrTranslator

import scala.collection.JavaConversions._

object Backend {

  def doGenerateFiles(state: GenerationState): Unit = {
    // TODO multifile classes support
    val psi2ir = new Psi2IrTranslator()
    if (state == null) return
    val psi2irContext = psi2ir.createGeneratorContext(state.getModule, state.getBindingContext)
    val irModuleFragment = psi2ir.generateModuleFragment(psi2irContext, state.getFiles)
    val jvmBackendContext = new JvmBackendContext(state, psi2irContext.getSourceManager, psi2irContext.getIrBuiltIns)
    try {
      for (irFile <- irModuleFragment.getFiles) {
        generateFile(irFile, jvmBackendContext)
        state.afterIndependentPart()
      }
    } catch {
      case e: Throwable =>
        CompilationErrorHandler.THROW_EXCEPTION.reportException(e, null) // TODO ktFile.virtualFile.url
    }
  }

  private def generateFile(irFile: IrFile, context: JvmBackendContext): Unit = {
    val lower = new JvmLower(context)
    val p = new Positioner(irFile)
    val codegen = new SJSIRCodegen(context)
//    lower.lower(irFile)
    for (c <- irFile.getDeclarations) c match {
      case i: IrClass => codegen.generate(i, p)
      case _ => throw new AssertionError("Declaration should be IrClass, got: " + c)
    }
  }

}
