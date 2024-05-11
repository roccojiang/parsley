package fix

import scala.meta._
import scalafix.v1._

class DebugShowTree extends SemanticRule("DebugShowTree") {
  override def fix(implicit doc: SemanticDocument): Patch = {
    show
  }
 }
