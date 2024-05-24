package parsley.garnish.rules.leftrec

import scala.collection.mutable
import scala.meta._
import scalafix.v1._

import parsley.garnish.analysis.ParserAnalyzer.{getNonTerminalParserDefns, ParserDefinition}
import parsley.garnish.model.{Function, Parser}
import parsley.garnish.model.Function._
import parsley.garnish.model.Parser._
import parsley.garnish.utils.TypeUtils.getParsleyType

object Transformation {
  def removeLeftRecursion(implicit doc: SemanticDocument): Patch = {
    val nonTerminals = getNonTerminalParserDefns.map { parserDefn =>
      parserDefn.name.symbol -> parserDefn
    }.to(mutable.Map)

    for (sym <- nonTerminals.keys) {
      val unfolded = unfold(nonTerminals.toMap, sym)
      val transformedParser = transform(unfolded, nonTerminals(sym).tpe)
      if (transformedParser.isDefined) {
        nonTerminals(sym) = nonTerminals(sym).copy(parser = transformedParser.get)
      }
    }

    nonTerminals.values.map {
      case ParserDefinition(_, transformed, _, originalTree) =>
          Patch.replaceTree(originalTree, transformed.term.syntax)
    }.asPatch
  }

  private case class UnfoldedProduction(empty: Option[Function], nonLeftRec: Parser, leftRec: Parser)

  /* Returns a parser transformed into postfix form if it is left-recursive, otherwise returns None. */
  private def transform(unfolded: UnfoldedProduction, tpe: Type.Name): Option[Parser] = {
    val UnfoldedProduction(empty, nonLeftRec, leftRec) = unfolded
    val empties = empty match {
      case None => Empty
      case Some(t) => Pure(t)
    }

    leftRec.normalise match {
      case Empty => None
      // case Pure(_) => None  // TODO: special case: report infinite loop which couldn't be left factored -- should this be looking for any parser which can parse empty?
      // TODO: import postfix if not in scope
      // TODO: report can't left factor if there are impure parsers
      case leftRec =>
        println(s">>> ${Postfix(tpe, nonLeftRec <|> empties, leftRec)}")
        Some(Postfix(tpe, nonLeftRec <|> empties, leftRec).normalise)
    }
  }

  private def unfold(env: Map[Symbol, ParserDefinition], nonTerminal: Symbol)(implicit doc: SemanticDocument): UnfoldedProduction = {

    def unfold0(visited: Set[Symbol], nt: Parser): UnfoldedProduction = {

      def unfoldNonTerminal(sym: Symbol): UnfoldedProduction = {
        val tpe = getParsleyType(sym)
        assert(tpe.isDefined, s"expected a Parsley type for $sym, got ${sym.info.get.signature}")

        if (sym == nonTerminal) {
          UnfoldedProduction(None, Empty, Pure(id))
        } else if (visited.contains(sym)) {
          UnfoldedProduction(None, NonTerminal(sym), Empty)
        } else {
          println(s"\tvisiting $sym")
          unfold0(visited + sym, env(sym).parser)
        }
      }

      def unfoldChoice(p: Parser, q: Parser): UnfoldedProduction = {
        val UnfoldedProduction(pe, pn, pl) = unfold0(visited, p)
        val UnfoldedProduction(qe, qn, ql) = unfold0(visited, q)

        // TODO: originally in the paper this was pe.xor(qe), but I think that's not true under PEG semantics?
        UnfoldedProduction(pe.orElse(qe), pn <|> qn, pl <|> ql)
      }

      def unfoldAp(p: Parser, q: Parser): UnfoldedProduction = {
        val UnfoldedProduction(pe, pn, pl) = unfold0(visited, p)
        val UnfoldedProduction(qe, qn, ql) = unfold0(visited, q)

        val empty = if (pe.isDefined && qe.isDefined) Some(App(pe.get, qe.get)) // pure f <*> pure x = pure (f x)
                    else None

        val lefts = {
          val llr = pl.map(flip) <*> q
          val rlr = pe match {
            case None => Empty
            case Some(f) => {
              ql.map(composeH(f))
            }
          }

          llr <|> rlr
        }

        val nonLefts = {
          val lnl = pn <*> q
          val rnl = pe match {
            case None => Empty
            case Some(f) => qn.map(f)
          }

          lnl <|> rnl
        }

        UnfoldedProduction(empty, nonLefts, lefts)
      }

      nt match {
        // TODO: this is a hack so that any single-arg Parsley combinators flagged as NTs are skipped - fix this!!!
        case NonTerminal(sym) if env contains sym => unfoldNonTerminal(sym)

        case _: Str => UnfoldedProduction(None, nt, Empty)
        case Pure(x) => UnfoldedProduction(Some(x), Empty, Empty)
        case Empty => UnfoldedProduction(None, Empty, Empty)

        case Many(p) =>
          val UnfoldedProduction(_, pn, pl) = unfold0(visited, p)

          val lefts = pl.map {
            val f = Var()
            val xs = Var()
            val nt = Var()

            // \f xs nt -> f nt : xs
            Lam(f, Lam(xs, Lam(nt, consH(App(f, nt), xs))))
          } <*> Many(p)

          val nonLefts = SomeP(pn)

          UnfoldedProduction(Some(Opaque(q"Nil")), nonLefts, lefts)

        case SomeP(p) => unfold0(visited, p.map(cons) <*> Many(p))

        case FMap(p, f) => unfold0(visited, Pure(f) <*> p)

        case p: LiftLike =>
          val liftedFunc: Parser = Pure(p.func match {
            case Opaque(f @ Term.Name(_), substs) if p.parsers.size > 1 => Opaque(q"$f.curried", substs)
            case _ => p.func
          })
          val curriedAp = p.parsers.foldLeft(liftedFunc)(_ <*> _)

          unfold0(visited, curriedAp)

        case Choice(p, q) => unfoldChoice(p, q)
        case Ap(p, q) => unfoldAp(p, q)

        // TODO
        case unhandled => UnfoldedProduction(None, unhandled, Empty)
      }
    }

    unfold0(Set.empty, env(nonTerminal).parser)
  }
}
