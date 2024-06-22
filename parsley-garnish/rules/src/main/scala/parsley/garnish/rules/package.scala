package parsley.garnish

import scala.meta._
import scalafix.v1._

import parsley.garnish.parser.GrammarExtractor.{getParserDefinitions, ParserDefinition}
import parsley.garnish.parser.Parser

package object rules {

  def rewriteAllParsers(rewriteFunc: Parser => Parser, sideEffects: Tree => Patch = Function.const(Patch.empty))
                       (implicit doc: SemanticDocument): Patch =
    getParserDefinitions().map { case ParserDefinition(_, parser, _, originalTree) =>
      val updatedParser = rewriteFunc(parser)
      if (!parser.isEquivalent(updatedParser)) {
        val updatedParserTerm = updatedParser.term.syntax
        sideEffects(originalTree).atomic
        Patch.replaceTree(originalTree, updatedParserTerm).atomic
      } else {
        Patch.empty
      }
    }.asPatch

  def show(implicit doc: SemanticDocument): Patch = {
    // println("Tree.syntax: " + doc.tree.syntax)
    println("Tree.structure: " + doc.tree.structure)
    // println("Tree.structureLabeled: " + doc.tree.structureLabeled)
    Patch.empty
  }
}
