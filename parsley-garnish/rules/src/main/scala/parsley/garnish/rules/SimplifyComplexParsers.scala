package parsley.garnish.rules

import scala.meta._
import scalafix.v1._

import parsley.garnish.analysis.ParserAnalyzer.{getAllParserDefns, ParserDefinition}
import parsley.garnish.utils._
import parsley.garnish.utils.TypeUtils.getSemanticType

class SimplifyComplexParsers extends SemanticRule("SimplifyComplexParsers") {
  override def fix(implicit doc: SemanticDocument): Patch = {
    // recursivelyPrintSignature(doc.tree)
    // recursivelyPrintSynthetics(doc.tree)

    getAllParserDefns.map { case ParserDefinition(_, parser, _, originalTree) =>
      println(parser)
      val simplifiedParser = parser.normalise
      if (simplifiedParser != parser) {
        val simplifiedParserTerm = simplifiedParser.term.syntax
        Patch.replaceTree(originalTree, simplifiedParserTerm)
      } else {
        Patch.empty
      }
    }.asPatch
  }
}
