package parsley.garnish.rules.leftrec

import scalafix.v1._

import parsley.garnish.analysis.ParserAnalyzer.{getNonTerminalParserDefns, ParserDefinition}
import Transformation.removeLeftRecursion

import parsley.garnish.utils.recursivelyPrintSynthetics

class FactorLeftRecursion(config: FactorLeftRecursionConfig) extends SemanticRule("FactorLeftRecursion") {
  def this() = this(FactorLeftRecursionConfig.default)

  override def withConfiguration(config: Configuration): metaconfig.Configured[Rule] = {
    config.conf
      .getOrElse("FactorLeftRecursion")(this.config)
      .map(newConfig => new FactorLeftRecursion(newConfig))
  }

  override def fix(implicit doc: SemanticDocument): Patch = {
    recursivelyPrintSynthetics(doc.tree)
    val leftRecFactoringPatches = removeLeftRecursion
    leftRecFactoringPatches + (if (config.reportNonTerminalLocations) lintNonTerminalLocations else Patch.empty)
  }

  private def lintNonTerminalLocations(implicit doc: SemanticDocument): Patch = {
    getNonTerminalParserDefns.map {
      case ParserDefinition(name, _, _, originalTree) =>
        Patch.lint(NonTerminalLint(originalTree.parent.getOrElse(originalTree), name.value))
    }.asPatch
  }
}
