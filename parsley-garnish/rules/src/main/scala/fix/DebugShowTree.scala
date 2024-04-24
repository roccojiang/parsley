package fix

import scala.meta._
import scalafix.v1._

import fix.utils.Matchers
import fix.utils.Parser.{Lift, Str}

class DebugShowTree extends SemanticRule("DebugShowTree") {
  override def fix(implicit doc: SemanticDocument): Patch = {

    doc.tree.traverse {
      case Matchers.liftExplicit(t) => {
        println(s" HELLO ${t}")
      }
      case Matchers.liftImplicit(t) => {
        println(s"IMPLICIT $t")
      }
    }

    val parser = Lift(q"Add", List(Str("hello")), isImplicit = true)
    println(parser.term.structure)

    // show
    Patch.empty
  }
 }
