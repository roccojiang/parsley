package parsley.garnish

import scala.meta._
import scalafix.v1._

object implicits {
  implicit class TreeOps(val tree: Tree) extends AnyVal {
    def containsAnyOf(terms: Term*): Boolean = tree.collect {
      case t: Term if terms.toSeq.exists(term => t.structure == term.structure) => t
    }.nonEmpty

    def isWithinScope(scope: Tree): Boolean = {
      tree.structure == scope.structure ||
        tree.parent.exists(_.isWithinScope(scope))
    }
  }
}
