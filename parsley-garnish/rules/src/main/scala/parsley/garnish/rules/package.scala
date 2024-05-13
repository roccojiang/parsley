package parsley.garnish

import scalafix.v1._
import scala.meta._

package object rules {
  def show(implicit doc: SemanticDocument): Patch = {
    println("Tree.syntax: " + doc.tree.syntax)
    println("Tree.structure: " + doc.tree.structure)
    println("Tree.structureLabeled: " + doc.tree.structureLabeled)
    Patch.empty
  }
}
