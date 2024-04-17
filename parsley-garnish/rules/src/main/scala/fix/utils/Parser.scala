package fix.utils

import scala.meta._
import scalafix.v1._

import NonTerminalDetection.NonTerminalTree

sealed abstract class Parser extends Product with Serializable
object Parser {
  // TODO: for everything that uses Term arguments, should we use a different type? Perhaps some way to represent functions, for defunctionalisation?
  final case class NonTerminal(ref: Symbol) extends Parser
  final case class Pure(a: Term) extends Parser
  final case object Empty extends Parser
  /* p: Parser[A], q: Parser[A] */
  final case class Or(p: Parser, q: Parser) extends Parser
  // TODO: generalise to lift2?
  /* p: Parser[A => B], q: Parser[A] */
  final case class <*>(p: Parser, q: Parser) extends Parser
  /* p: Parser[A], f: A => B */
  final case class FMap(p: Parser, f: Term) extends Parser
  final case class Many(p: Parser) extends Parser
  final case class Str(str: String) extends Parser

  final case object Unknown extends Parser
}

// The original term this parser is connected to. If None, then this parser is a result of a transformation
// TODO: better naming?
case class ParserWithTerm(parser: Parser, term: Term)
object ParserWithTerm {
  import Parser._

  def apply(term: Term)(implicit env: Map[Symbol, NonTerminalTree], doc: SemanticDocument): ParserWithTerm = {
    val parser = term match {
      // See https://scalacenter.github.io/scalafix/docs/developers/symbol-matcher.html#unapplytree for how to mitigate
      // against matching multiple times using SymbolMatchers

      case t if env.contains(t.symbol) => NonTerminal(t.symbol)

      // "string(s)"
      case Term.Apply.After_4_6_0(Matchers.string(_), Term.ArgClause(List(s), _)) => Str(s.toString)
      // "empty"
      case Matchers.empty(Term.Name(_)) => Empty
      // "pure(x)"
      case Term.Apply.After_4_6_0(Matchers.pure(_), Term.ArgClause(List(x), _)) => Pure(x)
      // "p.map(f)"
      case Term.Apply.After_4_6_0(Term.Select(qual, Matchers.map(_)), Term.ArgClause(List(f), _)) => FMap(apply(qual).parser, f)
      // "p <|> q" or "p | q"
      case Term.ApplyInfix.After_4_6_0(p, Matchers.<|>(_), _, Term.ArgClause(List(q), _)) => Or(apply(p).parser, apply(q).parser)
      // "p <*> q"
      case Term.ApplyInfix.After_4_6_0(p, Matchers.<*>(_), _, Term.ArgClause(List(q), _)) => <*>(apply(p).parser, apply(q).parser)

      case _ => Unknown
    }

    ParserWithTerm(parser, term)
  }
}
