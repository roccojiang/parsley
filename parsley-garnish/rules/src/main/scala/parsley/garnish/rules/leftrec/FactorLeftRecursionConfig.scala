package parsley.garnish.rules.leftrec

case class FactorLeftRecursionConfig(debugOptions: List[String] = List.empty) {
  def reportNonTerminalLocations: Boolean = debugOptions.contains("reportNonTerminalLocations")
}

object FactorLeftRecursionConfig {
  def default = FactorLeftRecursionConfig()

  implicit val surface = metaconfig.generic.deriveSurface[FactorLeftRecursionConfig]
  implicit val decoder = metaconfig.generic.deriveDecoder(default)
}
