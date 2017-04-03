package ch.epfl.k2sjsir.lower


import org.jetbrains.kotlin.backend.common.LowerKt._
import org.jetbrains.kotlin.backend.common.lower.{LocalFunctionsLowering, SharedVariablesLowering}
import org.jetbrains.kotlin.backend.jvm.JvmBackendContext
import org.jetbrains.kotlin.backend.jvm.lower._
import org.jetbrains.kotlin.ir.declarations.IrFile

class SJSIRLower(val context: JvmBackendContext) {

  def lower(irFile: IrFile) {
    new FileClassLowering(context).lower(irFile)
    new ConstAndJvmFieldPropertiesLowering().lower(irFile)
    new PropertiesLowering().lower(irFile)
    runOnFilePostfix(new InterfaceLowering(context.getState), irFile)
    runOnFilePostfix(new InterfaceDelegationLowering(context.getState), irFile)
    runOnFilePostfix(new SharedVariablesLowering(context), irFile)
    runOnFilePostfix(new InnerClassesLowering(context), irFile)
    runOnFilePostfix(new InnerClassConstructorCallsLowering(context), irFile)
    runOnFilePostfix(new LocalFunctionsLowering(context), irFile)
    runOnFilePostfix(new EnumClassLowering(context), irFile)
    //    runOnFilePostfix(new ObjectClassLowering(context), irFile)
    //    runOnFilePostfix(new InitializersLowering(context), irFile)
    runOnFilePostfix(new SingletonReferencesLowering(context), irFile)
    new SyntheticAccessorLowering(context.getState).lower(irFile)
    runOnFilePostfix(new BridgeLowering(context.getState), irFile)
    // ********************************************************
    // Own lowering phases: extract inner classes
    new ClassLowering().lower(irFile)
    new ConstructorLowering().lower(irFile)
  }

}
