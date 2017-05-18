package ch.epfl.k2sjsir.lower


import org.jetbrains.kotlin.psi.{KtDeclaration, KtFile}

class SJSIRLower() {

  def lower(ktFile: KtFile): List[KtDeclaration] = {
    new ClassLowering().lower(ktFile)
  }

}
