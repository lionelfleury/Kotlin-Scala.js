package ch.epfl.k2sjsir.translate

import org.jetbrains.kotlin.cli.common.CLIConfigurationKeys
import org.jetbrains.kotlin.cli.common.messages.CompilerMessageSeverity
import org.jetbrains.kotlin.diagnostics.DiagnosticUtils
import org.jetbrains.kotlin.js.translate.context.TranslationContext
import org.jetbrains.kotlin.psi.KtElement
import org.scalajs.core.ir.{Definitions, Position}
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types.{AnyType, ClassType}



trait IRNodeGen[T <: KtElement, TreeType <: IRNode] {
  def d: T
  implicit def c: TranslationContext
  implicit def pos: Position = {
    if (d == null) return Position.NoPosition

    val lc = DiagnosticUtils.getLineAndColumnInPsiFile(d.getContainingFile, d.getTextRange)
    val file = d.getContainingFile.getName
    Position(Position.SourceFile(file), lc.getLine - 1, lc.getColumn - 1)
  }

  def tree: TreeType

  private def notImplementedPrint(debugStr: String = "") = {
    val simpleName = getClass.getSimpleName
    val name = if (d != null) d.getClass.getSimpleName else ""
    val msg = s"Not supported $simpleName: $name" + (if (!debugStr.isEmpty) s" with message $debugStr" else "") + s" at pos : $pos"

    val messageCollector = c.getConfig.getConfiguration.getNotNull(CLIConfigurationKeys.MESSAGE_COLLECTOR_KEY)
    messageCollector.report(CompilerMessageSeverity.STRONG_WARNING, msg, null)
  }

  def notImplemented(debugStr: String = ""): Tree = {
    notImplementedPrint(debugStr)

    Debugger()
  }

  def notImplementedMemberDef(debugStr: String = ""): MemberDef = {
    notImplementedPrint(debugStr)
    FieldDef(false, Ident("debug"), AnyType, false)
  }

  def notImplementedReceiver(debugStr: String = ""): Tree = {
    notImplementedPrint(debugStr)
    This()(ClassType(Definitions.ObjectClass))
  }

}

trait Gen[T <: KtElement] extends IRNodeGen[T, Tree]
