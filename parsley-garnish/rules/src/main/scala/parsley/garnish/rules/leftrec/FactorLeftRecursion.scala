package parsley.garnish.rules.leftrec

import scalafix.v1._

import parsley.garnish.rules.leftrec.Transformation.removeLeftRecursion

class FactorLeftRecursion extends SemanticRule("FactorLeftRecursion") {
  override def fix(implicit doc: SemanticDocument): Patch = {
    removeLeftRecursion()
  }
}
