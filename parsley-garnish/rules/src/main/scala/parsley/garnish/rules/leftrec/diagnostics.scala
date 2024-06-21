package parsley.garnish.rules.leftrec

import scala.meta._
import scalafix.lint.LintSeverity
import scalafix.v1.Diagnostic

import parsley.garnish.parser.GrammarExtractor.ParserDefinition
import parsley.garnish.parser.Parser

case class LeftRecDerivesEmptyLint(parserDefn: ParserDefinition, transformed: Parser) extends Diagnostic {
  override def position: Position = parserDefn.definitionSite
  override def severity: LintSeverity = LintSeverity.Error
  override def message: String =
    s"""Left-recursion detected, but could not be removed from ${parserDefn.name.syntax}.
       |The resulting chain would be given a parser which consumes no input, causing it to loop indefinitely:
       |${transformed.term.syntax}
     """.stripMargin
}
