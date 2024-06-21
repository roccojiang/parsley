package parsley.garnish.rules.internal

import scala.meta._
import scalafix.lint.LintSeverity
import scalafix.v1.Diagnostic

import parsley.garnish.parser.GrammarExtractor.ParserDefinition

private[internal] case class DebugNonTerminalLint(parserDefn: ParserDefinition) extends Diagnostic {
  override def position: Position = parserDefn.definitionSite
  override def severity: LintSeverity = LintSeverity.Info
  override def message: String =
    s"""${parserDefn.name.syntax} was detected to be a non-terminal, and parsed as:
       |${parserDefn.parser}
     """.stripMargin
}
