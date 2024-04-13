package fix

import metaconfig.Configured
import scala.meta._
import scalafix.v1._
import scalafix.lint.LintSeverity

import fix.utils.isParsleyType

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

  private case class NonTerminalTree(name: Term.Name, body: Term, originalTree: Defn)

  private def getNonTerminals(implicit doc: SemanticDocument): Map[Symbol, NonTerminalTree] = {
    doc.tree.collect {
      // see https://scalameta.org/docs/semanticdb/specification.html#symbol for symbol uniqueness guarantees
      // since we only deal with one document at a time, it should be fine to look at both global and local symbols

      // TODO: refactor these pattern matches? highly repetitive
      // TODO: should handle the cases like "val x, y, z = ..." where the List[Pat.Var] is longer than 1?

      // value declarations inferred to have a Parsley[_] type
      case valDefn @ Defn.Val(_, List(Pat.Var(name)), _, body) if isParsleyType(valDefn.symbol) => {
        // println(s"$v: $rhs | ${rhs.getClass()} | ${rhs.synthetics}")
        // recursivelyPrintSynthetics(rhs)
        valDefn.symbol -> NonTerminalTree(name, body, valDefn)
      }

      case varDefn @ Defn.Var.After_4_7_2(_, List(Pat.Var(name)), _, body) if isParsleyType(varDefn.symbol) => {
        varDefn.symbol -> NonTerminalTree(name, body, varDefn)
      }

      case defDefn @ Defn.Def.After_4_7_3(_, name, _, _, body) if isParsleyType(defDefn.symbol) => {
        // TODO: do we need to do something special to deal with the function arguments?
        defDefn.symbol -> NonTerminalTree(name, body, defDefn)
      }
    }.toMap

    // TODO: WE CAN GET WHICH STRING IMPLICIT VIA SYNTHETICS
    // TODO: can we get the full qualified symbol of the e.g. stringLift function from the synthetics?
  }
}

case class NonTerminalLint(defn: Defn, name: String) extends Diagnostic {
  override def position: Position = defn.pos
  override def severity: LintSeverity = LintSeverity.Info
  override def message: String = s"$name was detected to be a non-terminal"
}
