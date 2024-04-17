package fix

import metaconfig.Configured
import scala.meta._
import scalafix.v1._
import scalafix.lint.LintSeverity

import utils.NonTerminalDetection.{NonTerminalTree, getNonTerminals}
import utils.ParserWithTerm

case class FactorLeftRecursionConfig(debugOptions: List[String] = List.empty) {
  def reportNonTerminalLocations: Boolean = debugOptions.contains("reportNonTerminalLocations")
}

object FactorLeftRecursionConfig {
  def default = FactorLeftRecursionConfig()
  implicit val surface = metaconfig.generic.deriveSurface[FactorLeftRecursionConfig]
  implicit val decoder = metaconfig.generic.deriveDecoder(default)
}

class FactorLeftRecursion(config: FactorLeftRecursionConfig) extends SemanticRule("FactorLeftRecursion") {
  def this() = this(FactorLeftRecursionConfig.default)

  override def withConfiguration(config: Configuration): Configured[Rule] = {
    config.conf
      .getOrElse("FactorLeftRecursion")(this.config)
      .map(newConfig => new FactorLeftRecursion(newConfig))
  }

  override def fix(implicit doc: SemanticDocument): Patch = {
    implicit val env = getNonTerminals
    val nonTerminals = env.map {
      case (sym, NonTerminalTree(_, body, _)) =>
        sym -> ParserWithTerm(body)
    }

    pprint.pprintln(nonTerminals)

    if (config.reportNonTerminalLocations) {
      lintNonTerminalLocations
    } else {
      Patch.empty
    }
  }

  private def lintNonTerminalLocations(implicit doc: SemanticDocument): Patch = {
    getNonTerminals.map {
      case (_, NonTerminalTree(name, _, originalTree)) => Patch.lint(NonTerminalLint(originalTree, name.value))
    }.asPatch
  }
}

case class NonTerminalLint(defn: Defn, name: String) extends Diagnostic {
  override def position: Position = defn.pos
  override def severity: LintSeverity = LintSeverity.Info
  override def message: String = s"$name was detected to be a non-terminal"
}
