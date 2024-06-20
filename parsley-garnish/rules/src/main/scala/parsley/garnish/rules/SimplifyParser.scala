package parsley.garnish.rules

import scala.meta._
import scalafix.v1._

import parsley.garnish.analysis.ParserTransformer.{getParserDefinitions, ParserDefinition}

class SimplifyParser extends SemanticRule("SimplifyParser") {
  override def fix(implicit doc: SemanticDocument): Patch = {
    getParserDefinitions(includeDefDefinitions = true).map { case ParserDefinition(_, parser, _, originalTree) =>
      val simplifiedParser = parser.prettify
      if (parser.normaliseExprs != simplifiedParser) {
        val simplifiedParserTerm = simplifiedParser.term.syntax
        Patch.replaceTree(originalTree, simplifiedParserTerm)
      } else {
        Patch.empty
      }
    }.asPatch
  }
}
