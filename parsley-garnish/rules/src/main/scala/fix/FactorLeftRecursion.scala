package fix

import metaconfig.Configured
import scala.collection.mutable
import scala.meta._
import scalafix.v1._
import scalafix.lint.LintSeverity

import utils.NonTerminalDetection.{NonTerminalTree, getNonTerminals}
import utils.Matchers
import utils.Parser, utils.Parser._

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
      case (nonTerminalSymbol, NonTerminalTree(_, bodyTerm, _)) =>
        nonTerminalSymbol -> Parser(bodyTerm)
    }.to(mutable.Map)

    for ((sym, parser) <- nonTerminals) {
      nonTerminals(sym) = transform(unfold(nonTerminals, sym))
    }

    // pprint.pprintln(nonTerminals)

    val leftRecFactoringPatches = nonTerminals.map {
      case (nt, transformed) => Patch.replaceTree(env(nt).body, transformed.term.syntax)
    }.asPatch

    leftRecFactoringPatches + (if (config.reportNonTerminalLocations) lintNonTerminalLocations else Patch.empty)
  }

  private def transform(unfolded: UnfoldedProduction): Parser = {
    val UnfoldedProduction(empty, nonLeftRec, leftRec) = unfolded
    val empties = empty match {
      case None    => Empty
      case Some(t) => Pure(t)
    }

    Postfix(nonLeftRec <|> empties, leftRec)
  }

  private def unfold(env: mutable.Map[Symbol, Parser], nonTerminal: Symbol)(implicit doc: SemanticDocument): UnfoldedProduction = {
    def unfold0(visited: Set[Symbol], nt: Parser): UnfoldedProduction = nt match {
      case p @ NonTerminal(sym) => {
        println(s"found a non-terminal: ${nt.term}")
        println(s"\tvisited = $visited")

        val tpe = utils.getType(sym.info.get.signature).collect {
          case TypeRef(_, Matchers.parsley(_), List(t)) => Type.Name(t.toString)
        }
        assert(tpe.isDefined, s"expected a Parsley type for $sym, got ${sym.info.get.signature}")

        if (sym == nonTerminal) {
          println(s"\tmatched non-terminal $sym")
          // TODO: is the type used here actually correct or am I just making this up?
          UnfoldedProduction(None, Empty, Pure(q"identity[${tpe.get}] _"))
        } else if (visited.contains(sym)) {
          println(s"\talready visited $sym")
          UnfoldedProduction(None, p, Empty)
        } else {
          println(s"\tvisiting $sym")
          unfold0(visited + sym, env(sym))
        }
      }

      case p @ Str(_) => UnfoldedProduction(None, p, Empty)
      case Pure(x) => UnfoldedProduction(Some(x), Empty, Empty)
      case Empty => UnfoldedProduction(None, Empty, Empty)

      case FMap(p, f) => unfold0(visited, Pure(f) <*> p)

      case Or(p, q) => {
        val UnfoldedProduction(pe, pn, pl) = unfold0(visited, p)
        val UnfoldedProduction(qe, qn, ql) = unfold0(visited, q)

        // TODO: originally in the paper this was pe.xor(qe), but I think that's not true under PEG semantics?
        UnfoldedProduction(pe.orElse(qe), pn <|> qn, pl <|> ql)
      }
      
      case r @ Ap(p, q) => {
        val UnfoldedProduction(pe, pn, pl) = unfold0(visited, p)
        val UnfoldedProduction(qe, qn, ql) = unfold0(visited, q)

        val empty = if (pe.isDefined && qe.isDefined) {
          // TODO: does this work as an implementation of the original bothEmpty? does it assume currying?
          Some(q"${pe.get}(${qe.get})") // pure f <*> pure x = pure (f x)
        } else {
          None
        }

        val lefts = {
          // TODO: implement flipping
          val llr = pl.map(q"flip(_)") <*> q
          val rlr = pe match {
            case None    => Empty
            case Some(f) => ql.map(q"$f.compose(_)")
          }
          
          llr <|> rlr
        }

        val nonLefts = {
          val lnl = pn <*> q
          val rnl = pe match {
            case None =>    Empty
            case Some(f) => qn.map(f)
          }
          
          lnl <|> rnl
        }

        UnfoldedProduction(empty, nonLefts, lefts)
      }

      // TODO
      case unhandled => UnfoldedProduction(None, unhandled, Empty)
    }

    unfold0(Set.empty, env(nonTerminal))
  }

  private def lintNonTerminalLocations(implicit doc: SemanticDocument): Patch = {
    getNonTerminals.map {
      case (_, NonTerminalTree(name, _, originalTree)) => Patch.lint(NonTerminalLint(originalTree, name.value))
    }.asPatch
  }
}

case class UnfoldedProduction(empty: Option[Term], nonLeftRec: Parser, leftRec: Parser)

case class NonTerminalLint(defn: Defn, name: String) extends Diagnostic {
  override def position: Position = defn.pos
  override def severity: LintSeverity = LintSeverity.Info
  override def message: String = s"$name was detected to be a non-terminal"
}
