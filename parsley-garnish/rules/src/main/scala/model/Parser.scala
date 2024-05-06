package model

import scala.meta._
import scalafix.v1._

import model.Func._, model.Parser._
import utils.Matchers
import fix.leftrec.NonTerminalDetection.NonTerminalTree

sealed abstract class Parser extends Product with Serializable {

  def term: Term

  def simplify: Parser = {

    /* p: Parser[B], f: B => C, p.map(f): Parser[C] */
    def simplifyMap(parser: FMap): Parser = {
      val FMap(p, f) = parser

      p match {
        case Empty => Empty
        case Pure(x) => Pure(App(f.simplify, x.simplify).simplify)
        // p.map(g).map(f) = p.map(f.compose(g))
        // g: D => B
        case FMap(p, g: Func) => {
          // FMap(p, App(App(compose[D, B, C], g), f))  // ???
          // val x = Var[D](Term.fresh())
          // FMap(p, Lam(x, App(f, App(g, x)))).simplify
          FMap(p, App(App(compose, g), f))
        }
        case p => FMap(p.simplify, f.simplify)
      }
    }

    this match {
      case Choice(p, Empty) => p.simplify
      case Choice(Empty, q) => q.simplify
      case Choice(Pure(f), _) => Pure(f.simplify)
      case Choice(p, q) => Choice(p.simplify, q.simplify)

      case Ap(Empty, _) => Empty
      case Ap(Pure(f), Pure(x)) => Pure(App(f.simplify, x.simplify).simplify)
      case Ap(Pure(f), x) => FMap(x.simplify, f.simplify)
      case Ap(p, q) => Ap(p.simplify, q.simplify)

      case p@FMap(_, _) => simplifyMap(p)

      // case Many(Empty)      => Empty
      // case Many(p)          => Many(p.simplify)

      case Postfix(tpe, p, op) => Postfix(tpe, p.simplify, op.simplify)

      // case LiftN(f, ps, isImplicit) => LiftN(f.simplify, ps.map(_.simplify), isImplicit)

      case Pure(f) => Pure(f.simplify)

      case _ => this
    }
  }

  override def toString: String = term.syntax
}

object Parser {
  final case class NonTerminal(val ref: Symbol)(implicit doc: SemanticDocument) extends Parser {
    val term = Term.Name(ref.info.get.displayName) // TODO: I think this is correct, but needs checking
  }

  /* x: A, pure(x): Parser[A] */
  final case class Pure(x: Func) extends Parser {
    val term = q"pure(${x.term})"
  }

  final case object Empty extends Parser {
    val term = Term.Name("empty")
  }

  /* p: Parser[A], q: Parser[A], p <|> q: Parser[A] */
  final case class Choice(p: Parser, q: Parser) extends Parser {
    // TODO: think about this
    // Note: this doesn't preserve the original combinator choice (i.e. <|> or |) but it doesn't really matter imo
    val term = q"${p.term} | ${q.term}"
  }

  // TODO: generalise to lift2?
  /* p: Parser[A => B], q: Parser[A], p <*> q: Parser[B] */
  final case class Ap(p: Parser, q: Parser) extends Parser {
    val term = q"${p.term} <*> ${q.term}"
  }

  /* p: Parser[A], f: Func[A => B], p.map(f): Parser[B] */
  final case class FMap(p: Parser, f: Func) extends Parser {
    val term = q"${p.term}.map(${f.term})"
  }

  /* p: Parser[A], many(p): Parser[List[A]] */
  final case class Many(p: Parser) extends Parser {
    val term = q"many(${p.term})"
  }

  /* string(s): Parser[String] */
  final case class Str(s: String) extends Parser {
    val term = q"string($s)"
  }

  /* p: Parser[A], op: Parser[A => A], p postfix op: Parser[A] */
  final case class Postfix(tpe: Type.Name, p: Parser, op: Parser) extends Parser {
    val term = q"chain.postfix[$tpe](${p.term})(${op.term})"
  }

  /* f: (T1, T2, ..., TN) => R, pN: Parser[TN], f.lift(ps) : Parser[R] */
  sealed trait LiftLike extends Parser {
    def func: Func
    def parsers: List[Parser]
  }

  final case class LiftImplicit(func: Func, parsers: List[Parser]) extends LiftLike {
    val term = q"${func.term}.lift(..${parsers.map(_.term)})"
  }

  final case class LiftExplicit(func: Func, parsers: List[Parser]) extends LiftLike {
    val term = {
      val liftN = Term.Name(s"lift${parsers.length}")
      q"$liftN(${func.term}, ..${parsers.map(_.term)})"
    }
  }

  final case class Zipped(func: Func, parsers: List[Parser]) extends LiftLike {
    val term = q"(..${parsers.map(_.term)}).zipped(${func.term})"
  }

  final case class Unknown(unrecognisedTerm: Term) extends Parser {
    val term = unrecognisedTerm
  }

  def apply(term: Term)(implicit env: Map[Symbol, NonTerminalTree], doc: SemanticDocument): Parser = term match {
    // See https://scalacenter.github.io/scalafix/docs/developers/symbol-matcher.html#unapplytree for how to mitigate
    // against matching multiple times using SymbolMatchers

    case t if env.contains(t.symbol) => NonTerminal(t.symbol)

    // "string(s)"
    case Term.Apply.After_4_6_0(Matchers.string(_), Term.ArgClause(List(str), _)) =>
      val s = str.text.stripPrefix("\"").stripSuffix("\"")
      Str(s)
    // "empty"
    case Matchers.empty(Term.Name(_)) => Empty
    // "pure(x)"
    case Term.Apply.After_4_6_0(Matchers.pure(_), Term.ArgClause(List(x), _)) => Pure(Opaque(x))
    // "p.map(f)"
    case Term.Apply.After_4_6_0(Term.Select(qual, Matchers.map(_)), Term.ArgClause(List(f), _)) =>
      val lambda = Func.buildFuncFromTerm(f, "MAP")
      FMap(apply(qual), lambda)
    // "p <|> q" or "p | q"
    case Term.ApplyInfix.After_4_6_0(p, Matchers.<|>(_), _, Term.ArgClause(List(q), _)) => Choice(apply(p), apply(q))
    // "p <*> q"
    case Term.ApplyInfix.After_4_6_0(p, Matchers.<*>(_), _, Term.ArgClause(List(q), _)) => Ap(apply(p), apply(q))

    // "liftN(f, p1, ..., pN)"
    case Term.Apply.After_4_6_0(Matchers.liftExplicit(_), Term.ArgClause(f :: ps, _)) =>
      val lambda = Func.buildFuncFromTerm(f, "LIFT2_EXPLICIT")
      LiftExplicit(lambda, ps.map(apply))

    // "f.lift(p1, ..., pN)"
    case Term.Apply.After_4_6_0(Term.Select(f, Matchers.liftImplicit(_)), Term.ArgClause(ps, _)) =>
      val func = f match {
        case Term.ApplyType.After_4_6_0(g, _) => g
        case _ => f
      }
      val lambda = Func.buildFuncFromTerm(func, "LIFT2_IMPLICIT")
      LiftImplicit(lambda, ps.map(apply))

    // "(p1, ..., pN).zipped(f)"
    case Term.Apply.After_4_6_0(Term.Select(Term.Tuple(ps), Matchers.zipped(_)), Term.ArgClause(List(f), _)) =>
      val lambda = Func.buildFuncFromTerm(f, "ZIPPED")
      Zipped(lambda, ps.map(apply))

    // TODO: pattern match on Apply, ApplyInfix so we can still try to find parsers within the term?
    case unrecognisedTerm => Unknown(unrecognisedTerm)
  }

  implicit class ParserOps(p: Parser) {
    def <*>(q: Parser): Parser = Ap(p, q).simplify

    def <|>(q: Parser): Parser = Choice(p, q).simplify

    def map(f: Func): Parser = FMap(p, f).simplify
  }
}
