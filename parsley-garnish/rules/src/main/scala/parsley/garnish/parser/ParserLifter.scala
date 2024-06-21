package parsley.garnish.parser

import scala.PartialFunction.cond
import scala.meta._
import scalafix.v1._

import parsley.garnish.implicits.TermOps
import parsley.garnish.parser.Parser._

object ParserLifter {

  def lift(term: Term)(implicit doc: SemanticDocument): Parser = term match {
    /* Core parsers (excluding non-terminals) */
    case Term.Apply.After_4_6_0(matchers.pure(_), Term.ArgClause(List(func), _)) =>
      Pure(func.toExpr())
    case matchers.empty(Term.Name(_)) =>
      Empty
    case Term.ApplyInfix.After_4_6_0(p, matchers.choice(_), _, Term.ArgClause(List(q), _)) =>
      p.toParser | q.toParser
    case Term.ApplyInfix.After_4_6_0(p, matchers.ap(_), _, Term.ArgClause(List(q), _)) =>
      p.toParser <*> q.toParser

    /* Lifting parsers */
    case Term.Apply.After_4_6_0(Term.Select(qual, matchers.map(_)), Term.ArgClause(List(func), _)) =>
      FMap(qual.toParser, func.toExpr())
    case Term.Apply.After_4_6_0(matchers.liftExplicit(_), Term.ArgClause(f :: ps, _)) =>
      LiftExplicit(f.toExpr(ps.size), ps.map(_.toParser))
    case Term.Apply.After_4_6_0(Term.Select(f, matchers.liftImplicit(_)), Term.ArgClause(ps, _)) =>
      val func = f match {
        case Term.ApplyType.After_4_6_0(g, _) => g
        case _ => f
      }
      LiftImplicit(func.toExpr(ps.size), ps.map(_.toParser))
    case Term.Apply.After_4_6_0(Term.Select(Term.Tuple(ps), matchers.zipped(_)), Term.ArgClause(List(f), _)) =>
      Zipped(f.toExpr(ps.size), ps.map(_.toParser))
    case Term.Apply.After_4_6_0(func, ps) if func.synthetics.exists(cond(_) {
        case SelectTree(_, IdTree(symInfo)) => matchers.bridge.matches(symInfo.symbol)
    }) =>
      Bridge(func.toExpr(ps.size), ps.map(_.toParser)) // directly mapping over the ArgClause without unpacking it seems to work fine

    /* Character parsers */
    case Term.Apply.After_4_6_0(matchers.string(_), Term.ArgClause(List(Lit.String(str)), _)) =>
      Str(str, implicitSyntax = false)
    case s @ Lit.String(str) if s.synthetics.exists(cond(_) {
      case ApplyTree(IdTree(symInfo), _) => matchers.stringLift.matches(symInfo.symbol)
    }) => Str(str, implicitSyntax = true)
    case Term.Apply.After_4_6_0(matchers.char(_), Term.ArgClause(List(Lit.Char(chr)), _)) =>
      Chr(chr, implicitSyntax = false)
    case s @ Lit.Char(chr) if s.synthetics.exists(cond(_) {
      case ApplyTree(IdTree(symInfo), _) => matchers.charLift.matches(symInfo.symbol)
    }) => Chr(chr, implicitSyntax = true)

    /* Sequencing parsers */
    case Term.ApplyInfix.After_4_6_0(p, matchers.then(_), _, Term.ArgClause(List(q), _)) =>
      p.toParser ~> q.toParser
    case Term.ApplyInfix.After_4_6_0(p, matchers.thenDiscard(_), _, Term.ArgClause(List(q), _)) =>
      p.toParser <~ q.toParser

    /* Iterative parsers */
    case Term.Apply.After_4_6_0(matchers.many(_), Term.ArgClause(List(p), _)) =>
      ManyP(p.toParser)
    case Term.Apply.After_4_6_0(matchers.some(_), Term.ArgClause(List(p), _)) =>
      SomeP(p.toParser)

    /* Separated values parsers */
    case Term.Apply.After_4_6_0(matchers.endBy(_), Term.ArgClause(List(p, sep), None)) =>
      EndBy(p.toParser, sep.toParser)

    /* Non-terminals */
    case t: Term.Name if !(t.symbol.owner.value startsWith "parsley/") =>
      NonTerminal(t.symbol, t.symbol.info.get.displayName)

    /* Otherwise, unknown parser */
    case unrecognised => Unknown(unrecognised)
  }
}
