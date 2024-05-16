package parsley.garnish

import scalafix.v1._
import scala.meta._

package object utils {
  def recursivelyPrintSynthetics(tree: Tree)(implicit doc: SemanticDocument): Unit = {
    tree match {
      case term: Term =>
        val synthetics = term.synthetics
        if (synthetics.nonEmpty) println(s"> SYNTHETICS $term: $synthetics")
      case _ =>
    }
    tree.children.foreach(recursivelyPrintSynthetics)
  }

  def recursivelyPrintSymbol(tree: Tree)(implicit doc: SemanticDocument): Unit = {
    println(s"> SYMBOL $tree: ${tree.symbol}")
    tree.children.foreach(recursivelyPrintSymbol)
  }
}
