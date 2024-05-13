package parsley.garnish.rules.leftrec

import scala.meta._
import scalafix.lint.LintSeverity
import scalafix.v1.Diagnostic

case class NonTerminalLint(defn: Defn, name: String) extends Diagnostic {
  override def position: Position = defn.pos
  override def severity: LintSeverity = LintSeverity.Info
  override def message: String = s"$name was detected to be a non-terminal"
}
