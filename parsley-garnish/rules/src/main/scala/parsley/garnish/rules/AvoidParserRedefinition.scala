package parsley.garnish.rules

import scala.meta._
import scalafix.v1._

import parsley.garnish.parser.Parser._
import parsley.garnish.parser.ParserLifter.{getParserDefinitions, ParserDefinition}
import parsley.garnish.parser.Parser

class AvoidParserRedefinition extends SemanticRule("AvoidParserRedefinition") {
  override def fix(implicit doc: SemanticDocument): Patch = {
    getParserDefinitions(includeDefDefinitions = true).map { case ParserDefinition(_, parser, _, originalTree) =>
      val updatedParser = parser.rewrite(redefinitions)
      if (!parser.isEquivalent(updatedParser)) {
        val updatedParserTerm = updatedParser.term.syntax
        Patch.replaceTree(originalTree, updatedParserTerm)
      } else {
        Patch.empty
      }
    }.asPatch
  }

  def redefinitions: PartialFunction[Parser, Parser] = {
    case ManyP(p <~ sep) => EndBy(p, sep)
  }
}
