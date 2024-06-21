package parsley.garnish.parser

import scala.meta._
import scalafix.v1._

import parsley.garnish.expr.Expr, Expr._

/**
  * The root type of the intermediate Parser AST.
  * Modelled as an expanded version of the Parsley combinator tree, representing all composite combinators as well.
  */
sealed abstract class Parser extends Product with Serializable {
  import Parser._

  def term: Term = ParserLowerer.lower(this)

  def isEquivalent(other: Parser): Boolean = this.normalise == other.normalise

  /* Used for equivalence checking */
  def normalise: Parser = simplify.normaliseExprs

  /* Simplify parsers and attempt to resugar them */
  def prettify: Parser = normalise.resugar

  /* Simplification via parser laws */
  def simplify: Parser = this.rewrite {
    case p <|> Empty         => p
    case Empty <|> q         => q
    case Pure(f) <|> _       => Pure(f)

    case Empty <*> _         => Empty
    case Pure(f) <*> Pure(x) => Pure(App(f, x))
    case Pure(f) <*> x       => FMap(x, f)

    // empty.map(f) == empty  [proof in report appendix]
    case FMap(Empty, _)      => Empty
    // pure(x).map(f) == pure(f) <*> pure(x) == pure(f(x))
    case FMap(Pure(x), f)    => Pure(App(f, x))
    // p.map(f).map(g) == p.map(g compose f)
    case FMap(FMap(p, f), g) => FMap(p, compose(g, f))
  }

  /* Resugaring */
  def resugar: Parser = this.rewrite {
    // Generic resugaring rules that are run on all parsers

    // p.map(\x -> \y -> y) <*> q == p ~> q
    // TODO: is there still a chance that \x.\y.body has a single-term Translucent body rather than a Var?
    case FMap(p, Abs(_, Abs(Var(y, _), Var(z, _)))) <*> q if (y == z) => p ~> q
    // p.map(\x -> \y -> x) <*> q == p <~ q
    case FMap(p, Abs(Var(x, _), Abs(_, Var(z, _)))) <*> q if (x == z) => p <~ q

    // p.map(f.curried) <*> q == (p, q).zipped(f)
    // TODO: up to 22 args
    case FMap(p1, Abs(x1, Abs(x2, body))) <*> p2 =>
      Zipped(AbsN(List(x1, x2), body), List(p1, p2))
    case FMap(p1, Abs(x1, Abs(x2, Abs(x3, body)))) <*> p2 <*> p3 =>
      Zipped(AbsN(List(x1, x2, x3), body), List(p1, p2, p3))
    case FMap(p1, Abs(x1, Abs(x2, Abs(x3, Abs(x4, body))))) <*> p2 <*> p3 <*> p4 =>
      Zipped(AbsN(List(x1, x2, x3, x4), body), List(p1, p2, p3, p4))

  }.transform {
    // Generic resugaring rules that should only be applied once, since RHS constructors overlap with LHS constructors
    // If applied using rewrite, it would never terminate

    // Scala 2 cannot resolve implicit stringLifts in some positions
    case FMap(Str(s, _), f) => FMap(Str(s, implicitSyntax = false), f)
    case Zipped(f, Str(s, _) :: ps) => Zipped(f, Str(s, implicitSyntax = false) :: ps)
  }

  def normaliseExprs: Parser = transformExprs(_.normalise)
  def etaReduceExprs: Parser = transformExprs(_.etaReduce)

  def transformExprs(f: Expr => Expr): Parser = this.transform {
    case Pure(x) => Pure(f(x))
    case FMap(p, func) => FMap(p, f(func))
    case As(p, x) => As(p, f(x))
    case LiftImplicit(func, parsers) => LiftImplicit(f(func), parsers)
    case LiftExplicit(func, parsers) => LiftExplicit(f(func), parsers)
    case Zipped(func, parsers) => Zipped(f(func), parsers)
    case Bridge(func, parsers) => Bridge(f(func), parsers)
  }

  // Bottom-up transformation
  def transform(pf: PartialFunction[Parser, Parser]): Parser = {
    val parser = this match {
      case p <|> q => <|>(p.transform(pf), q.transform(pf))
      case p <*> q => <*>(p.transform(pf), q.transform(pf))
      case p ~> q  => ~>(p.transform(pf), q.transform(pf))
      case p <~ q  => <~(p.transform(pf), q.transform(pf))
      case FMap(p, f) => FMap(p.transform(pf), f)
      case As(p, x) => As(p.transform(pf), x)
      case ManyP(p) => ManyP(p.transform(pf))
      case SomeP(p) => SomeP(p.transform(pf))
      case Left1(tpe, p, op) => Left1(tpe, p.transform(pf), op.transform(pf))
      case Postfix(tpe, p, op) => Postfix(tpe, p.transform(pf), op.transform(pf))
      case LiftImplicit(f, ps) => LiftImplicit(f, ps.map(_.transform(pf)))
      case LiftExplicit(f, ps) => LiftExplicit(f, ps.map(_.transform(pf)))
      case Zipped(f, ps) => Zipped(f, ps.map(_.transform(pf)))
      case Bridge(f, ps) => Bridge(f, ps.map(_.transform(pf)))
      case EndBy(p, sep) => EndBy(p.transform(pf), sep.transform(pf))

      case s: Str => s
      case c: Chr => c
      case Digit => Digit
      case p: Pure => p
      case Empty => Empty
      case nt: NonTerminal => nt
      case unk: Unknown => unk
    }

    pf.applyOrElse(parser, identity[Parser])
  }

  // Transformation to normal form in a bottom-up manner
  def rewrite(pf: PartialFunction[Parser, Parser]): Parser = {
    def pf0(p: Parser) = if (pf isDefinedAt p) pf(p).rewrite(pf) else p

    this.transform(pf0)
  }
}

object Parser {

  sealed trait CoreParser extends Parser
  final case class NonTerminal(ref: Symbol, name: String) extends CoreParser
  final case class Pure(x: Expr /* Expr[A] */) extends CoreParser /* Parser[A] */
  final case object Empty extends CoreParser /* Parser[Nothing] */
  final case class <|>(p: Parser /* Parser[A] */, q: Parser /* Parser[A] */) extends CoreParser /* Parser[A] */
  final case class <*>(p: Parser /* Parser[A => B] */, q: Parser /* Parser[A] */) extends CoreParser /* Parser[B] */

  sealed trait ResultChangingParser extends Parser
  final case class FMap(p: Parser /* Parser[A] */, func: Expr /* Expr[A => B] */) extends ResultChangingParser /* Parser[B] */
  final case class As(p: Parser /* Parser[_] */, x: Expr /* Expr[A] */) extends ResultChangingParser /* Parser[A] */

  sealed trait LiftParser extends Parser {
    val func: Expr
    val parsers: List[Parser]
  }
  /* func: Expr[(T1, ..., TN) => R], p1: Parser[T1], pN: Parser[TN]*/
  final case class LiftExplicit(func: Expr, parsers: List[Parser]) extends LiftParser /* Parser[R] */
  final case class LiftImplicit(func: Expr, parsers: List[Parser]) extends LiftParser /* Parser[R] */
  final case class Zipped(func: Expr, parsers: List[Parser]) extends LiftParser /* Parser[R] */
  final case class Bridge(func: Expr, parsers: List[Parser]) extends LiftParser /* Parser[R] */

  sealed trait CharacterParser extends Parser
  final case class Str(s: Expr /* Expr[String] */, implicitSyntax: Boolean = false) extends CharacterParser /* Parser[String] */
  final case class Chr(c: Expr /* Expr[Char] */, implicitSyntax: Boolean = false) extends CharacterParser /* Parser[Char] */
  final case object Digit extends CharacterParser /* Parser[Char] */

  sealed trait SequenceParser extends Parser
  final case class ~>(p: Parser /* Parser[_] */, q: Parser /* Parser[A] */) extends SequenceParser /* Parser[A] */
  final case class <~(p: Parser /* Parser[A] */, q: Parser /* Parser[_] */) extends SequenceParser /* Parser[A] */

  sealed trait ChainParser extends Parser
  final case class Postfix(tpe: Option[Type.Name], p: Parser /* Parser[A] */, op: Parser /* Parser[A => A] */) extends ChainParser /* Parser[A] */
  final case class Left1(tpe: Option[Type.Name], p: Parser /* Parser[A] */, op: Parser /* Parser[(A, A) => A] */) extends ChainParser /* Parser[A] */

  sealed trait IterativeParser extends Parser
  final case class ManyP(p: Parser /* Parser[A] */) extends IterativeParser /* Parser[List[A]] */
  final case class SomeP(p: Parser /* Parser[A] */) extends IterativeParser /* Parser[List[A]] */

  sealed trait SeparatedValuesParser extends Parser
  final case class EndBy(p: Parser /* Parser[A] */, sep: Parser /* Parser[_] */) extends SeparatedValuesParser /* Parser[List[A]] */

  final case class Unknown(unrecognisedTerm: Term) extends Parser

  /* Extension methods for parsers */
  implicit class ParserOps(private val p: Parser) extends AnyVal {
    def <*>(q: Parser): <*> = Parser.<*>(p, q)
    def ~>(q: Parser): ~> = Parser.~>(p, q)
    def <~(q: Parser): <~ = Parser.<~(p, q)
    def |(q: Parser): <|> = <|>(p, q)
    def map(f: Expr): FMap = FMap(p, f)
  }

  implicit class MultiParserOps(private val ps: List[Parser]) extends AnyVal {
    def zipped(f: Expr): Parser = Zipped(f, ps)
  }
}
