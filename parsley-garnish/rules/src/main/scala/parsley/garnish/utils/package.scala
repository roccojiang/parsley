package parsley.garnish

import scalafix.v1._
import scala.meta._

package object utils {
  def recursivelyPrintSynthetics(tree: Tree)(implicit doc: SemanticDocument): Unit = {
    tree match {
      case term: Term => println(s"synthetics $term: ${term.synthetics}")
      case _ =>
    }
    tree.children.foreach(recursivelyPrintSynthetics)
  }
}
