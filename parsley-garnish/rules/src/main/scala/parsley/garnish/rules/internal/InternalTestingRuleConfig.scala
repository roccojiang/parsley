package parsley.garnish.rules.internal

private[internal] case class InternalTestingRuleConfig(debugOptions: List[String] = List.empty) {
  def reportNonTerminalLocations: Boolean = debugOptions.contains("reportNonTerminalLocations")
}

private[internal] object InternalTestingRuleConfig {
  def default = InternalTestingRuleConfig()

  implicit val surface = metaconfig.generic.deriveSurface[InternalTestingRuleConfig]
  implicit val decoder = metaconfig.generic.deriveDecoder(default)
}
