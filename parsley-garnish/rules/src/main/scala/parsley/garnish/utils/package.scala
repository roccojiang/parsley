package parsley.garnish

import scalafix.v1._
import scala.meta._

package object utils {
  def recursivelyPrintSynthetics(tree: Tree)(implicit doc: SemanticDocument): Unit = {
    tree match {
      case term: Term =>
        val synthetics = term.synthetics
        if (synthetics.nonEmpty)
          println(s"> SYNTHETICS ${term.structure}: ${synthetics.structure}")
      case _ =>
    }
    tree.children.foreach(recursivelyPrintSynthetics)
  }

  def recursivelyPrintSymbol(tree: Tree)(implicit doc: SemanticDocument): Unit = {
    println(s"> SYMBOL $tree: ${tree.symbol}")
    tree.children.foreach(recursivelyPrintSymbol)
  }

  def recursivelyPrintSignature(tree: Tree)(implicit doc: SemanticDocument): Unit = {
    val signature = tree.symbol.info.map(_.signature.structure)
    if (signature.isDefined)
      println(s"> SIGNATURE ${tree.syntax.takeWhile(_ != '\n')}: ${signature.get}")
      tree.children.foreach(recursivelyPrintSignature)
  }

  def printInfo(t: Tree, msg: String = "")(implicit doc: SemanticDocument): Unit = {
    import Console._
    println(s"${RED}$msg >>${RESET} ${t.structure}:")
    println(s"\t${BLUE}symbol =${RESET} ${t.symbol} ${BLUE}|${RESET} ${t.symbol.info.map(_.signature.structureLabeled)}")
    if (t.isInstanceOf[Term]) {
      val term = t.asInstanceOf[Term]
      println(s"\t${GREEN}synthetics =${RESET} ${term.synthetics} ${GREEN}|${RESET} ${term.synthetics.structure}")
    }
  }
}
