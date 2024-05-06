package fix.leftrec

import scala.collection.mutable
import scala.meta._
import scalafix.v1._

import model.{Func, Parser}
import model.Func._, model.Parser._
import utils.getParsleyType
import NonTerminalDetection.{NonTerminalTree, getNonTerminals}

object Transformation {
  def removeLeftRecursion(implicit doc: SemanticDocument): Patch = {
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

    nonTerminals.map {
      case (nt, transformed) => Patch.replaceTree(env(nt).body, transformed.term.syntax)
    }.asPatch
  }

  private case class UnfoldedProduction(empty: Option[Func], nonLeftRec: Parser, leftRec: Parser)

  /* Returns a parser transformed into postfix form if it is left-recursive, otherwise returns None. */
  private def transform(unfolded: UnfoldedProduction, tpe: Type.Name): Option[Parser] = {
    val UnfoldedProduction(empty, nonLeftRec, leftRec) = unfolded
    val empties = empty match {
      case None => Empty
      case Some(t) => Pure(t)
    }

    leftRec match {
      case Empty => None
      // case Pure(_) => None  // TODO: special case: report infinite loop which couldn't be left factored
      // TODO: import postfix if not in scope
      case _ => Some(Postfix(tpe, nonLeftRec <|> empties, leftRec).simplify)
    }
  }

  private def unfold(env: Map[Symbol, Parser], nonTerminal: Symbol)(implicit doc: SemanticDocument): UnfoldedProduction = {

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
          unfold0(visited + sym, env(sym))
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

        val empty = if (pe.isDefined && qe.isDefined) {
          // TODO: does this work as an implementation of the original bothEmpty? does it assume currying?
          Some(App(pe.get, qe.get)) // pure f <*> pure x = pure (f x)
        } else {
          None
        }

        val lefts = {
          val llr = pl.map(flip) <*> q
          val rlr = pe match {
            case None => Empty
            case Some(f) => ql.map(App(compose, f))
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
        case NonTerminal(sym) => unfoldNonTerminal(sym)

        case p@Str(_) => UnfoldedProduction(None, p, Empty)
        case Pure(x) => UnfoldedProduction(Some(x), Empty, Empty)
        case Empty => UnfoldedProduction(None, Empty, Empty)

        case FMap(p, f) => unfold0(visited, Ap(Pure(f), p))

        // TODO: these need to be uncurried, now that Funcs are represented properly, arglist lengths will not match up
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

        case Choice(p, q) => unfoldChoice(p, q)
        case Ap(p, q) => unfoldAp(p, q)

        // TODO
        case unhandled => UnfoldedProduction(None, unhandled, Empty)
      }
    }

    unfold0(Set.empty, env(nonTerminal))
  }
}
