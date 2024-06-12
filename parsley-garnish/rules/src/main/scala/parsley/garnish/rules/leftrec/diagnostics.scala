package parsley.garnish.rules.leftrec

import scala.meta._
import scalafix.lint.LintSeverity
import scalafix.v1.Diagnostic

import parsley.garnish.model.Parser
import parsley.garnish.analysis.ParserTransformer.ParserDefinition

case class DebugNonTerminalLint(parserDefn: ParserDefinition) extends Diagnostic {
  override def position: Position = parserDefn.definitionSite
  override def severity: LintSeverity = LintSeverity.Info
  override def message: String =
    s"""${parserDefn.name.syntax} was detected to be a non-terminal, and parsed as:
       |${parserDefn.parser}
     """.stripMargin
}

case class LeftRecDerivesEmptyLint(parserDefn: ParserDefinition, transformed: Parser) extends Diagnostic {
  override def position: Position = parserDefn.definitionSite
  override def severity: LintSeverity = LintSeverity.Error
  override def message: String =
    s"""Left-recursion could not be removed from ${parserDefn.name.syntax}.
       |The resulting chain would be given a parser which consumes no input, causing it to loop indefinitely:
       |${transformed.term.syntax}
     """.stripMargin
}
