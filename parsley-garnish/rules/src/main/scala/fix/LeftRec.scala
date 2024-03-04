package fix

import scalafix.v1._
import scala.meta._

class LeftRec extends SemanticRule("LeftRec") {
  override def fix(implicit doc: SemanticDocument): Patch = {
    show
  }
}
