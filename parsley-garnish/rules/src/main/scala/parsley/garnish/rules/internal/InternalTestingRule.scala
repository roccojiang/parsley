package parsley.garnish.rules.internal

import scalafix.v1._

import parsley.garnish.parser.GrammarExtractor.getParserDefinitions

/**
  * This rule is only used for internal integration testing purposes.
  */
private[internal] class InternalTestingRule(config: InternalTestingRuleConfig) extends SemanticRule("InternalTestingRule") {
  def this() = this(InternalTestingRuleConfig.default)

  override def withConfiguration(config: Configuration): metaconfig.Configured[Rule] = {
    config.conf
      .getOrElse("InternalTestingRule")(this.config)
      .map(newConfig => new InternalTestingRule(newConfig))
  }

  override def fix(implicit doc: SemanticDocument): Patch = {
    if (config.reportNonTerminalLocations) lintNonTerminalLocations else Patch.empty
  }

  private def lintNonTerminalLocations(implicit doc: SemanticDocument): Patch =
    getParserDefinitions(includeDefDefinitions = false).map(defn => Patch.lint(DebugNonTerminalLint(defn))).asPatch
}
