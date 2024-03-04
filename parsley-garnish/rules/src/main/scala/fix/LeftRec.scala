package fix

import scalafix.v1._
import scala.meta._

import fix.utils.getNonTerminals

class LeftRec extends SemanticRule("LeftRec") {
  override def fix(implicit doc: SemanticDocument): Patch = {
    getNonTerminals
    Patch.empty
  }
}
