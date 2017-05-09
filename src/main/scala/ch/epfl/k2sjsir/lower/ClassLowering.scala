package ch.epfl.k2sjsir.lower

import org.jetbrains.kotlin.psi.{KtClassOrObject, KtDeclaration, KtFile}

import scala.collection.JavaConverters._

class ClassLowering {

  def lower(f: KtFile): List[KtDeclaration] = {
    f.getDeclarations.asScala.flatMap {
      case c: KtClassOrObject => lower(c)
      case x => List(x)
    }.toList
  }

  def lower(c: KtClassOrObject) : List[KtDeclaration] = {
    c :: c.getDeclarations.asScala.collect {
      case x: KtClassOrObject => x
    }.flatMap(lower).toList
  }
}
