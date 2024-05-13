package parsley.garnish.rules.leftrec

import scalafix.v1._

import NonTerminalDetection.{getNonTerminals, ParserDefinition}
import Transformation.removeLeftRecursion

class FactorLeftRecursion(config: FactorLeftRecursionConfig) extends SemanticRule("FactorLeftRecursion") {
  def this() = this(FactorLeftRecursionConfig.default)

  override def withConfiguration(config: Configuration): metaconfig.Configured[Rule] = {
    config.conf
      .getOrElse("FactorLeftRecursion")(this.config)
      .map(newConfig => new FactorLeftRecursion(newConfig))
  }

  override def fix(implicit doc: SemanticDocument): Patch = {
    val leftRecFactoringPatches = removeLeftRecursion
    leftRecFactoringPatches + (if (config.reportNonTerminalLocations) lintNonTerminalLocations else Patch.empty)
  }

  private def lintNonTerminalLocations(implicit doc: SemanticDocument): Patch = {
    getNonTerminals.map {
      case (_, ParserDefinition(name, originalTree, _, _)) =>
        Patch.lint(NonTerminalLint(originalTree.parent.getOrElse(originalTree), name.value))
    }.asPatch
  }
}
