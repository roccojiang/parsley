package parsley.garnish.rules

import scala.meta._
import scalafix.v1._

import parsley.garnish.analysis.ParserTransformer.{getAllParserDefns, ParserDefinition}
import parsley.garnish.utils.recursivelyPrintSymbol
import scalafix.lint.LintSeverity

class SimplifyComplexParsers extends SemanticRule("SimplifyComplexParsers") {
  override def fix(implicit doc: SemanticDocument): Patch = {
    getAllParserDefns.map { case ParserDefinition(_, parser, _, originalTree) =>
      val simplifiedParser = parser.simplify
      if (simplifiedParser != parser) {
        val simplifiedParserTerm = simplifiedParser.term.syntax
        Patch.replaceTree(originalTree, simplifiedParserTerm)
      } else {
        Patch.empty
      }
    }.asPatch
  }
}
