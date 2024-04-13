package fix.utils

import scala.meta._

sealed trait Parser

object Parser {
  // TODO: for everything that uses Term arguments, should we use a different type?
  case class NonTerminal(ref: Symbol) extends Parser
  case class Pure(a: Term) extends Parser
  case object Empty extends Parser
  /* p: Parser[A], q: Parser[A] */
  case class <|>(p: Parser, q: Parser) extends Parser
  /* p: Parser[A => B], q: Parser[A] */
  case class <*>(p: Parser, q: Parser) extends Parser
  /* p: Parser[A], f: A => B */
  case class Map(p: Parser, f: Term) extends Parser
  case class Many(p: Parser) extends Parser
  case class Str(str: String) extends Parser

  // The original term this parser is connected to. If None, then this parser is a result of a transformation
  // TODO: better naming? Also: do we have a case when term is None, should this not be an Option?
  case class ParserWithTerm(parser: Parser, term: Option[Term])
}
