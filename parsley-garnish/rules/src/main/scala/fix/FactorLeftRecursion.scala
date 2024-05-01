package fix

import metaconfig.Configured
import scala.collection.mutable
import scala.meta._
import scalafix.v1._
import scalafix.lint.LintSeverity

import utils.Func, utils.Func._
import utils.NonTerminalDetection.{NonTerminalTree, getNonTerminals}
import utils.Parser, utils.Parser._

case class FactorLeftRecursionConfig(debugOptions: List[String] = List.empty) {
  def reportNonTerminalLocations: Boolean = debugOptions.contains("reportNonTerminalLocations")
}

object FactorLeftRecursionConfig {
  def default = FactorLeftRecursionConfig()
  implicit val surface = metaconfig.generic.deriveSurface[FactorLeftRecursionConfig]
  implicit val decoder = metaconfig.generic.deriveDecoder(default)
}

//noinspection ScalaStyle
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
      case (nonTerminalSymbol, NonTerminalTree(_, bodyTerm, _, _)) =>
        nonTerminalSymbol -> Parser(bodyTerm)
    }.to(mutable.Map)
    
    for ((sym, parser) <- nonTerminals) {
      val unfolded = unfold(nonTerminals.toMap, sym)
      val transformedParser = transform(unfolded, env(sym).tpe)
      if (transformedParser.isDefined) {
        nonTerminals(sym) = transformedParser.get
      }
    }
    
    pprint.pprintln(nonTerminals)
    
    val leftRecFactoringPatches = nonTerminals.map {
      case (nt, transformed) => Patch.replaceTree(env(nt).body, transformed.term.syntax)
    }.asPatch

    leftRecFactoringPatches + (if (config.reportNonTerminalLocations) lintNonTerminalLocations else Patch.empty)
  }

  /* Returns a parser transformed into postfix form if it is left-recursive, otherwise returns None. */
  private def transform[A](unfolded: UnfoldedProduction[A, A], tpe: Type.Name): Option[Parser[A]] = {
    val UnfoldedProduction(empty, nonLeftRec, leftRec) = unfolded
    val empties = empty match {
      case None    => Empty
      case Some(t) => Pure(t)
    }

    leftRec match {
      case Empty   => None
      // case Pure(_) => None  // TODO: special case: report infinite loop which couldn't be left factored
      case _       => Some(Postfix(tpe, nonLeftRec <|> empties, leftRec).simplify)
    }
  }

  private def unfold[T](env: Map[Symbol, Parser[_]], nonTerminal: Symbol)(implicit doc: SemanticDocument): UnfoldedProduction[T, T] = {
    def unfold0[A](visited: Set[Symbol], nt: Parser[A]): UnfoldedProduction[T, A] = {

      def unfoldAp[B, C](p: Parser[B => C], q: Parser[B]): UnfoldedProduction[T, C] = {
          val UnfoldedProduction(pe, pn, pl) = unfold0(visited, p)
          val UnfoldedProduction(qe, qn, ql) = unfold0(visited, q)

          val empty = if (pe.isDefined && qe.isDefined) {
            // TODO: does this work as an implementation of the original bothEmpty? does it assume currying?
            Some(App(pe.get, qe.get, isMethod = false)) // pure f <*> pure x = pure (f x)
          } else {
            None
          }

          val lefts = {
            // TODO: implement flipping
            // pprint.pprintln(s"pl for $r = ${pl.term.syntax}")

            val llr = {
              val f = Var[T => B => C](Term.fresh())
              val x = Var[B](Term.fresh())
              val y = Var[T](Term.fresh())
              // pl.map(Lam(f, Flip(f))) <*> q
              // TODO: formulate flip and compose in terms of lambda calculus instead?
              pl.map(Lam(f, Lam(x, Lam(y, App(App(f, y), x))))) <*> q
            }
            val rlr = pe match {
              case None    => Empty
              case Some(f) => {
                val g = Var[T => B](Term.fresh())
                ql.map(Lam(g, Compose(f, g)))
              }
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

      nt match {
        case p @ NonTerminal(sym) => {
          println(s"found a non-terminal: ${nt.term}")
          println(s"\tvisited = $visited")

          val tpe = utils.getSymbolType(sym)
          assert(tpe.isDefined, s"expected a Parsley type for $sym, got ${sym.info.get.signature}")

          if (sym == nonTerminal) {
            println(s"\tmatched non-terminal $sym")
            // TODO: is the type used here actually correct or am I just making this up?
            UnfoldedProduction(None, Empty, Pure(Id(tpe.get).asInstanceOf[Func[T => A]]))
          } else if (visited.contains(sym)) {
            println(s"\talready visited $sym")
            UnfoldedProduction(None, p, Empty)
          } else {
            println(s"\tvisiting $sym")
            unfold0(visited + sym, env(sym).asInstanceOf[Parser[A]])
          }
        }

        case p @ Str(_) => UnfoldedProduction(None, p, Empty)
        case Pure(x) => UnfoldedProduction(Some(x), Empty, Empty)
        case Empty => UnfoldedProduction(None, Empty, Empty)

        case FMap(p, f) => unfold0(visited, Ap(Pure(f), p))

        case Lift2(f, p, q, _) => {
          unfold0(visited, Pure(f) <*> p <*> q)
        }
        case Lift3(f, p, q, r, _) => {
          unfold0(visited, Pure(f) <*> p <*> q <*> r)
        }

        // TODO: don't just convert this into curried form with a chain of <*>s?
        // case LiftN(f, ps, _) => {
        //   // val parsers: List[Parser[A]] = Pure(f) +: ps
        //   // val curried = parsers.reduceLeft((x: Parser[A => A], y: Parser[A]) => x <*> y)
        //   val curried = ps.foldLeft(Pure(f))(_ <*> _)
        //   unfold0(visited, curried)
        // }

        case Choice(p, q) => {
          val UnfoldedProduction(pe, pn, pl) = unfold0(visited, p)
          val UnfoldedProduction(qe, qn, ql) = unfold0(visited, q)

          // TODO: originally in the paper this was pe.xor(qe), but I think that's not true under PEG semantics?
          UnfoldedProduction(pe.orElse(qe), pn <|> qn, pl <|> ql)
        }
      
        case Ap(p, q) => {
          unfoldAp(p, q)
        }

        // TODO
        case unhandled => UnfoldedProduction(None, unhandled, Empty)
      }
    }

    unfold0(Set.empty, env(nonTerminal).asInstanceOf[Parser[T]])
  }

  private def lintNonTerminalLocations(implicit doc: SemanticDocument): Patch = {
    getNonTerminals.map {
      case (_, NonTerminalTree(name, _, _, originalTree)) => Patch.lint(NonTerminalLint(originalTree, name.value))
    }.asPatch
  }
}

case class UnfoldedProduction[T, A](empty: Option[Func[A]], nonLeftRec: Parser[A], leftRec: Parser[T => A])

case class NonTerminalLint(defn: Defn, name: String) extends Diagnostic {
  override def position: Position = defn.pos
  override def severity: LintSeverity = LintSeverity.Info
  override def message: String = s"$name was detected to be a non-terminal"
}
