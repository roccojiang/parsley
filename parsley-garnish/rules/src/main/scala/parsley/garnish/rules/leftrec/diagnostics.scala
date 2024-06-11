package parsley.garnish.rules.leftrec

import scala.meta._
import scalafix.lint.LintSeverity
import scalafix.v1.Diagnostic

import parsley.garnish.model.Parser
import parsley.garnish.analysis.ParserTransformer.ParserDefinition

case class NonTerminalLint(tree: Tree, name: String) extends Diagnostic {
  override def position: Position = tree.pos
  override def severity: LintSeverity = LintSeverity.Info
  override def message: String = s"$name was detected to be a non-terminal."
}

case class LeftRecDerivesEmptyLint(parserDefn: ParserDefinition, transformed: Parser) extends Diagnostic {
  override def position: Position = parserDefn.originalTree.pos
  override def severity: LintSeverity = LintSeverity.Error
  override def message: String =
    s"""Left-recursion could not be removed from ${parserDefn.name.syntax}.
       |The resulting chain would be given a parser which consumes no input, causing it to loop indefinitely:
       |${transformed.term.syntax}
     """.stripMargin
}
