package fix.utils

import scala.meta._
import scalafix.v1._

import NonTerminalDetection.NonTerminalTree

sealed abstract class Parser extends Product with Serializable {
  def term: Term
}
object Parser {
  final case class NonTerminal(val ref: Symbol)(implicit doc: SemanticDocument) extends Parser {
    val term = Term.Name(ref.info.get.displayName) // TODO: I think this is correct, but needs checking
  }

  final case class Pure(x: Term) extends Parser {
    val term = q"pure($x)"
  }

  final case object Empty extends Parser {
    val term = Term.Name("empty")
  }

  final case class Or(p: Parser, q: Parser) extends Parser {
    // TODO: think about this
    // Note: this doesn't preserve the original combinator choice (i.e. <|> or |) but it doesn't really matter imo
    val term = q"${p.term} | ${q.term}"
  }

  // TODO: generalise to lift2?
  // final case class Ap[A, B](p: Parser, q: Parser) extends Parser {
  final case class Ap(p: Parser, q: Parser) extends Parser {
    val term = q"${p.term} <*> ${q.term}"
  }

  final case class FMap(p: Parser, f: Term) extends Parser {
    val term = q"${p.term}.map($f)"
  }

  final case class Many(p: Parser) extends Parser {
    val term = q"many(${p.term})"
  }

  final case class Str(s: String) extends Parser {
    val term = q"string($s)"
  }

  final case class Postfix(p: Parser, op: Parser) extends Parser {
    val term = q"chain.postfix(${p.term})(${op.term})"
  }

  final case class Lift(f: Term, ps: List[Parser], isImplicit: Boolean) extends Parser {
    val n = ps.length

    val term = if (isImplicit) {
      q"$f.lift(..${ps.map(_.term)})"
    } else {
      val lift = Term.Name(s"lift$n")
      q"$lift($f, ..${ps.map(_.term)})"
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
    case Term.Apply.After_4_6_0(Matchers.pure(_), Term.ArgClause(List(x), _)) => Pure(x)
    // "p.map(f)"
    case Term.Apply.After_4_6_0(Term.Select(qual, Matchers.map(_)), Term.ArgClause(List(f), _)) => FMap(apply(qual), f)
    // "p <|> q" or "p | q"
    case Term.ApplyInfix.After_4_6_0(p, Matchers.<|>(_), _, Term.ArgClause(List(q), _)) => Or(apply(p), apply(q))
    // "p <*> q"
    case Term.ApplyInfix.After_4_6_0(p, Matchers.<*>(_), _, Term.ArgClause(List(q), _)) => Ap(apply(p), apply(q))

    // "liftN(f, p1, ..., pN)"
    case Term.Apply.After_4_6_0(Matchers.liftExplicit(_), Term.ArgClause(f :: ps, _)) =>
      Lift(f, ps.map(apply).toList, isImplicit = false)
    // "f.lift(p1, ..., pN)"
    case Term.Apply.After_4_6_0(Term.Select(f, Matchers.liftImplicit(_)), Term.ArgClause(ps, _)) =>
      Lift(f, ps.map(apply).toList, isImplicit = true)

    // TODO: pattern match on Apply, ApplyInfix so we can still try to find parsers within the term?
    case unrecognisedTerm => Unknown(unrecognisedTerm)
  }

  implicit class ParserOps(p: Parser) {
    def <|>(q: Parser): Parser = (p, q) match {
      case (p, Empty)   => p
      case (Empty, q)   => q
      case (Pure(_), _) => p
      case (p, q)       => Or(p, q)
    }

    def <*>(q: Parser): Parser = (p, q) match {
      case (Empty, _) => Empty
      case (p, q)     => Ap(p, q)
    }

    def map(f: Term): Parser = (p, f) match {
      case (Empty, _) => Empty // TODO: does this hold?
      case (p, f)     => FMap(p, f)
    }
  }
}
