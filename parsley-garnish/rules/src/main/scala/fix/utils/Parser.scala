package fix.utils

import scala.meta._
import scalafix.v1._

import Func._
import NonTerminalDetection.NonTerminalTree

sealed abstract class Parser[+A] extends Product with Serializable {
  import Parser._
  
  def term: Term

  def simplify: Parser[A] = {

    def simplifyMap[B, C](p: Parser[B], f: Func[B => C]): Parser[C] = (p, f) match {
      case (Empty, _) => Empty // TODO: does this hold?
      // case (p, Id(_)) => p.simplify
      // case (Pure(x), f) => Pure(App(f.simplify, x.simplify, isMethod = false).simplify)
      case (Pure(x), f) => Pure(App(f.simplify, x.simplify).simplify)
      case (p, f) => FMap(p.simplify, f.simplify)
    }

    this match {
      case Choice(p, Empty)   => p.simplify
      case Choice(Empty, q)   => q.simplify
      case Choice(Pure(f), _) => Pure(f.simplify)
      case Choice(p, q)       => Choice(p.simplify, q.simplify)

      case Ap(Empty, _)         => Empty
      case Ap(Pure(f), Pure(x)) => Pure(App(f.simplify, x.simplify).simplify)
      case Ap(Pure(f), x)       => FMap(x.simplify, f.simplify)
      case Ap(p, q)             => Ap(p.simplify, q.simplify)

      case FMap(p, f) => simplifyMap(p, f)

      // case Many(Empty)      => Empty
      // case Many(p)          => Many(p.simplify)

      case Postfix(tpe, p, op)    => Postfix(tpe, p.simplify, op.simplify)

      // case LiftN(f, ps, isImplicit) => LiftN(f.simplify, ps.map(_.simplify), isImplicit)

      case Pure(f) => Pure(f.simplify)

      case _ => this
    }
  }
}
object Parser {
  final case class NonTerminal[A](val ref: Symbol)(implicit doc: SemanticDocument) extends Parser[A] {
    val term = Term.Name(ref.info.get.displayName) // TODO: I think this is correct, but needs checking
  }

  final case class Pure[A](x: Func[A]) extends Parser[A] {
    val term = q"pure(${x.term})"
  }

  final case object Empty extends Parser {
    val term = Term.Name("empty")
  }

  final case class Choice[A](p: Parser[A], q: Parser[A]) extends Parser[A] {
    // TODO: think about this
    // Note: this doesn't preserve the original combinator choice (i.e. <|> or |) but it doesn't really matter imo
    val term = q"${p.term} | ${q.term}"
  }

  // TODO: generalise to lift2?
  // final case class Ap[A, B](p: Parser, q: Parser) extends Parser {
  final case class Ap[A, B](p: Parser[A => B], q: Parser[A]) extends Parser[B] {
    val term = q"${p.term} <*> ${q.term}"
  }

  final case class FMap[A, B](p: Parser[A], f: Func[A => B]) extends Parser[B] {
    val term = q"${p.term}.map(${f.term})"
  }

  final case class Many[A](p: Parser[A]) extends Parser[List[A]] {
    val term = q"many(${p.term})"
  }

  final case class Str(s: String) extends Parser {
    val term = q"string($s)"
  }

  final case class Postfix[A](tpe: Type.Name, p: Parser[A], op: Parser[A => A]) extends Parser[A] {
    val term = q"chain.postfix[$tpe](${p.term})(${op.term})"
  }

  sealed trait LiftN[A] extends Parser[A]
  final case class Lift2[A, B, R](f: Func[A => B => R], p1: Parser[A], p2: Parser[B], isImplicit: Boolean) extends LiftN[R] {
    val term = if (isImplicit) {
      q"${f.term}.lift(${p1.term}, ${p2.term})"
    } else {
      q"lift2(${f.term}, ${p1.term}, ${p2.term})"
    }
  }
  final case class Lift3[A, B, C, R](f: Func[A => B => C => R], p1: Parser[A], p2: Parser[B], p3: Parser[C], isImplicit: Boolean) extends LiftN[R] {
    val term = if (isImplicit) {
      q"${f.term}.lift(${p1.term}, ${p2.term}, ${p3.term})"
    } else {
      q"lift3(${f.term}, ${p1.term}, ${p2.term}, ${p3.term})"
    }
  }

  // TODO: this is 1. homogeneous 2. really wrong lmao
  // final case class LiftN[A, B](f: Func[A => B], ps: List[Parser[A]], isImplicit: Boolean) extends Parser[B] {
  //   val n = ps.length

  //   val term = if (isImplicit) {
  //     q"${f.term}.lift(..${ps.map(_.term)})"
  //   } else {
  //     val lift = Term.Name(s"lift$n")
  //     q"$lift(${f.term}, ..${ps.map(_.term)})"
  //   }
  // }

  final case class Unknown(unrecognisedTerm: Term) extends Parser {
    val term = unrecognisedTerm
  }

  def apply[A](term: Term)(implicit env: Map[Symbol, NonTerminalTree], doc: SemanticDocument): Parser[A] = term match {
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
    case Term.Apply.After_4_6_0(Matchers.pure(_), Term.ArgClause(List(x), _)) => Pure(Opaque(x))
    // "p.map(f)"
    case Term.Apply.After_4_6_0(Term.Select(qual, Matchers.map(_)), Term.ArgClause(List(f), _)) => FMap(apply(qual), Opaque(f))
    // "p <|> q" or "p | q"
    case Term.ApplyInfix.After_4_6_0(p, Matchers.<|>(_), _, Term.ArgClause(List(q), _)) => Choice(apply(p), apply(q))
    // "p <*> q"
    case Term.ApplyInfix.After_4_6_0(p, Matchers.<*>(_), _, Term.ArgClause(List(q), _)) => Ap(apply(p), apply(q))

    // "liftN(f, p1, ..., pN)"
    case Term.Apply.After_4_6_0(Matchers.liftExplicit(_), Term.ArgClause(f :: ps, _)) if (ps.length == 2) => {
      Lift2(Opaque(f), apply(ps(0)), apply(ps(1)), isImplicit = false)
    }
    case Term.Apply.After_4_6_0(Matchers.liftExplicit(_), Term.ArgClause(f :: ps, _)) if (ps.length == 3) => {
      Lift3(Opaque(f), apply(ps(0)), apply(ps(1)), apply(ps(2)), isImplicit = false)
    }

    // "f.lift(p1, ..., pN)"
    case Term.Apply.After_4_6_0(Term.Select(f, Matchers.liftImplicit(_)), Term.ArgClause(ps, _)) if (ps.length == 2) =>
      Lift2(Opaque(f), apply(ps(0)), apply(ps(1)), isImplicit = true)
    case Term.Apply.After_4_6_0(Term.Select(f, Matchers.liftImplicit(_)), Term.ArgClause(ps, _)) if (ps.length == 3) =>
      Lift3(Opaque(f), apply(ps(0)), apply(ps(1)), apply(ps(2)), isImplicit = true)

    // TODO: pattern match on Apply, ApplyInfix so we can still try to find parsers within the term?
    case unrecognisedTerm => Unknown(unrecognisedTerm)
  }

  implicit class ParserOps[A](p: Parser[A]) {
    def <|>(q: Parser[A]): Parser[A] = Choice(p, q).simplify
  }
  implicit class ParserOps2[A, B](p: Parser[A => B]) {
    def <*>(q: Parser[A]): Parser[B] = Ap(p, q).simplify
  }
  implicit class ParserOps3[A, B](p: Parser[A]) {
    def map(f: Func[A => B]): Parser[B] = FMap(p, f).simplify
  }

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
