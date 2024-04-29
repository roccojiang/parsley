package fix.utils

import scala.meta._
import scalafix.v1._

import Func._
import NonTerminalDetection.NonTerminalTree

sealed abstract class Parser extends Product with Serializable {
  import Parser._
  
  def term: Term

  def simplify: Parser = this match {
    case Choice(p, Empty)   => p.simplify
    case Choice(Empty, q)   => q.simplify
    case Choice(Pure(f), _) => Pure(f.simplify)
    case Choice(p, q)       => Choice(p.simplify, q.simplify)

    case Ap(Empty, _)         => Empty
    case Ap(Pure(f), Pure(x)) => Pure(App(f.simplify, x.simplify).simplify)
    // case Ap(Pure(f), x)       => FMap(x.simplify, f.simplify)
    case Ap(p, q)             => Ap(p.simplify, q.simplify)

    case FMap(Empty, _)   => Empty // TODO: does this hold?
    case FMap(p, Id(_))   => p.simplify
    case FMap(Pure(x), f) => Pure(App(f.simplify, x.simplify).simplify)
    case FMap(p, f)       => FMap(p.simplify, f.simplify)

    case Many(Empty)      => Empty
    case Many(p)          => Many(p.simplify)

    case Postfix(tpe, p, op)    => Postfix(tpe, p.simplify, op.simplify)

    case LiftN(f, ps, isImplicit) => LiftN(f.simplify, ps.map(_.simplify), isImplicit)

    case Pure(f) => Pure(f.simplify)

    case _ => this
  }
}
object Parser {
  final case class NonTerminal(val ref: Symbol)(implicit doc: SemanticDocument) extends Parser {
    val term = Term.Name(ref.info.get.displayName) // TODO: I think this is correct, but needs checking
  }

  final case class Pure(x: Func) extends Parser {
    val term = q"pure(${x.term})"
  }

  final case object Empty extends Parser {
    val term = Term.Name("empty")
  }

  final case class Choice(p: Parser, q: Parser) extends Parser {
    // TODO: think about this
    // Note: this doesn't preserve the original combinator choice (i.e. <|> or |) but it doesn't really matter imo
    val term = q"${p.term} | ${q.term}"
  }

  // TODO: generalise to lift2?
  // final case class Ap[A, B](p: Parser, q: Parser) extends Parser {
  final case class Ap(p: Parser, q: Parser) extends Parser {
    val term = q"${p.term} <*> ${q.term}"
  }

  final case class FMap(p: Parser, f: Func) extends Parser {
    val term = q"${p.term}.map(${f.term})"
  }

  final case class Many(p: Parser) extends Parser {
    val term = q"many(${p.term})"
  }

  final case class Str(s: String) extends Parser {
    val term = q"string($s)"
  }

  final case class Postfix(tpe: Type.Name, p: Parser, op: Parser) extends Parser {
    val term = q"chain.postfix[$tpe](${p.term})(${op.term})"
  }

  final case class LiftN(f: Func, ps: List[Parser], isImplicit: Boolean) extends Parser {
    val n = ps.length

    val term = if (isImplicit) {
      q"${f.term}.lift(..${ps.map(_.term)})"
    } else {
      val lift = Term.Name(s"lift$n")
      q"$lift(${f.term}, ..${ps.map(_.term)})"
    }
  }

  final case class Unknown(unrecognisedTerm: Term) extends Parser {
    val term = unrecognisedTerm
  }

  def apply(term: Term)(implicit env: Map[Symbol, NonTerminalTree], doc: SemanticDocument): Parser = term match {
    // See https://scalacenter.github.io/scalafix/docs/developers/symbol-matcher.html#unapplytree for how to mitigate
    // against matching multiple times using SymbolMatchers

    case t if env.contains(t.symbol) => NonTerminal(t.symbol)

    // "string(s)"
    case Term.Apply.After_4_6_0(Matchers.string(_), Term.ArgClause(List(str), _)) => {
      val s = str.text.stripPrefix("\"").stripSuffix("\"")
      Str(s)
    }
    // "empty"
    case Matchers.empty(Term.Name(_)) => Empty
    // "pure(x)"
    case Term.Apply.After_4_6_0(Matchers.pure(_), Term.ArgClause(List(x), _)) => Pure(Opaque(x, shouldCurry = false))
    // "p.map(f)"
    case Term.Apply.After_4_6_0(Term.Select(qual, Matchers.map(_)), Term.ArgClause(List(f), _)) => FMap(apply(qual), Opaque(f, shouldCurry = false))
    // "p <|> q" or "p | q"
    case Term.ApplyInfix.After_4_6_0(p, Matchers.<|>(_), _, Term.ArgClause(List(q), _)) => Choice(apply(p), apply(q))
    // "p <*> q"
    case Term.ApplyInfix.After_4_6_0(p, Matchers.<*>(_), _, Term.ArgClause(List(q), _)) => Ap(apply(p), apply(q))

    // "liftN(f, p1, ..., pN)"
    case Term.Apply.After_4_6_0(Matchers.liftExplicit(_), Term.ArgClause(f :: ps, _)) =>
      LiftN(Opaque(f, shouldCurry = ps.length > 1), ps.map(apply).toList, isImplicit = false)
    // "f.lift(p1, ..., pN)"
    case Term.Apply.After_4_6_0(Term.Select(f, Matchers.liftImplicit(_)), Term.ArgClause(ps, _)) =>
      LiftN(Opaque(f, shouldCurry = ps.length > 1), ps.map(apply).toList, isImplicit = true)

    // TODO: pattern match on Apply, ApplyInfix so we can still try to find parsers within the term?
    case unrecognisedTerm => Unknown(unrecognisedTerm)
  }

  implicit class ParserOps(p: Parser) {
    def <|>(q: Parser): Parser = Choice(p, q).simplify
    def <*> (q: Parser): Parser = Ap(p, q).simplify
    def map(f: Func): Parser = FMap(p, f).simplify

    // def <|>(q: Parser): Parser = (p, q) match {
    //   case (p, Empty)   => p
    //   case (Empty, q)   => q
    //   case (Pure(_), _) => p
    //   case (p, q)       => Choice(p, q)
    // }

    // def <*>(q: Parser): Parser = (p, q) match {
    //   case (Empty, _)         => Empty
    //   case (Pure(f), Pure(x)) => Pure(App(f, x))
    //   case (p, q)             => Ap(p, q)
    // }

    // def map(f: Func): Parser = (p, f) match {
    //   case (Empty, _) => Empty // TODO: does this hold?
    //   case (p, Id(_)) => p
    //   case (p, f)     => FMap(p, f)
    // }
  }
}
