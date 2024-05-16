package parsley.garnish.model

import scala.PartialFunction.cond
import scala.meta._
import scalafix.v1._

import Function._
import parsley.garnish.utils.Matchers

sealed abstract class Parser extends Product with Serializable {
  import Parser._

  def term: Term

  def simplify: Parser = transform(this) {
    // p <|> empty == p
    case Choice(p, Empty) => p
    // empty <|> q == q
    case Choice(Empty, q) => q
    // pure(f) <|> u == pure(f)
    case Choice(Pure(f), _) => Pure(f)

    // empty <*> u == empty
    case Ap(Empty, _) => Empty
    // pure(f) <*> pure(x) == pure(f(x))
    case Ap(Pure(f), Pure(x)) => Pure(App(f, x))
    // pure(f) <*> x == x.map(f)
    case Ap(Pure(f), x) => FMap(x, f)

    // TODO: prove empty.map(f) == empty
    // possibly via empty <*> pure x == empty and mf <*> pure x == pure ($ x) <*> mf
    // or by parametricity
    case FMap(Empty, _) => Empty
    // pure(x).map(f) == pure(f) <*> pure(x) == pure(f(x))
    case FMap(Pure(x), f) => Pure(App(f, x))
    // p.map(f).map(g) == p.map(g compose f)
    case FMap(FMap(p, f), g) => FMap(p, composeH(g, f))
  }

  // override def toString: String = term.syntax
}

object Parser {
  final case class NonTerminal(val ref: Symbol)(implicit doc: SemanticDocument) extends Parser {
    val term = Term.Name(ref.info.get.displayName) // TODO: I think this is correct, but needs checking
  }

  /* x: A, pure(x): Parser[A] */
  final case class Pure(x: Function) extends Parser {
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
  final case class FMap(p: Parser, f: Function) extends Parser {
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
    def func: Function
    def parsers: List[Parser]
  }

  final case class LiftImplicit(func: Function, parsers: List[Parser]) extends LiftLike {
    val term = q"${func.term}.lift(..${parsers.map(_.term)})"
  }

  final case class LiftExplicit(func: Function, parsers: List[Parser]) extends LiftLike {
    val term = {
      val liftN = Term.Name(s"lift${parsers.length}")
      q"$liftN(${func.term}, ..${parsers.map(_.term)})"
    }
  }

  final case class Zipped(func: Function, parsers: List[Parser]) extends LiftLike {
    val term = q"(..${parsers.map(_.term)}).zipped(${func.term})"
  }
  final case class Bridge(func: Function, parsers: List[Parser]) extends LiftLike {
    val term = q"${func.term}(..${parsers.map(_.term)})"
  }

  final case class Unknown(unrecognisedTerm: Term) extends Parser {
    val term = unrecognisedTerm
  }

  def apply(term: Term)(implicit doc: SemanticDocument): Parser = term match {
    // See https://scalacenter.github.io/scalafix/docs/developers/symbol-matcher.html#unapplytree for how to mitigate
    // against matching multiple times using SymbolMatchers

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
      val lambda = Function.buildFuncFromTerm(f, "MAP")
      FMap(apply(qual), lambda)
    // "p <|> q" or "p | q"
    case Term.ApplyInfix.After_4_6_0(p, Matchers.<|>(_), _, Term.ArgClause(List(q), _)) => Choice(apply(p), apply(q))
    // "p <*> q"
    case Term.ApplyInfix.After_4_6_0(p, Matchers.<*>(_), _, Term.ArgClause(List(q), _)) => Ap(apply(p), apply(q))

    // "liftN(f, p1, ..., pN)"
    case Term.Apply.After_4_6_0(Matchers.liftExplicit(_), Term.ArgClause(f :: ps, _)) =>
      val lambda = Function.buildFuncFromTerm(f, "LIFT2_EXPLICIT")
      LiftExplicit(lambda, ps.map(apply))

    // "f.lift(p1, ..., pN)"
    case Term.Apply.After_4_6_0(Term.Select(f, Matchers.liftImplicit(_)), Term.ArgClause(ps, _)) =>
      val func = f match {
        case Term.ApplyType.After_4_6_0(g, _) => g
        case _ => f
      }
      val lambda = Function.buildFuncFromTerm(func, "LIFT2_IMPLICIT")
      LiftImplicit(lambda, ps.map(apply))

    // "(p1, ..., pN).zipped(f)"
    case Term.Apply.After_4_6_0(Term.Select(Term.Tuple(ps), Matchers.zipped(_)), Term.ArgClause(List(f), _)) =>
      val lambda = Function.buildFuncFromTerm(f, "ZIPPED")
      Zipped(lambda, ps.map(apply))
    
    // "BridgeCons(p1, ..., pN)"
    case Term.Apply.After_4_6_0(fun, ps) if fun.synthetics.exists(cond(_) {
        case SelectTree(_, IdTree(symInfo)) => Matchers.bridgeApply.matches(symInfo.symbol)
    }) =>
      val lambda = Function.buildFuncFromTerm(fun, "BRIDGE")
      Bridge(lambda, ps.map(apply)) // TODO: I haven't unpacked the ArgClause here and directly mapped, does this work?

    // any other unrecognised term names will be assumed to be a non-terminal
    // this is a conservative approach, it might assume some Parsley combinators are actually NTs?
    case t: Term.Name => NonTerminal(t.symbol)

    // TODO: pattern match on Apply, ApplyInfix so we can still try to find parsers within the term?
    case unrecognisedTerm => Unknown(unrecognisedTerm)
  }

  private def transform(p: Parser)(pf: PartialFunction[Parser, Parser]): Parser = {
    if (pf.isDefinedAt(p)) {
      transform(pf(p))(pf) // apply pf, then recurse over the result
    } else p match {
      case Choice(p, q) => Choice(transform(p)(pf), transform(q)(pf))
      case Ap(p, q) => Ap(transform(p)(pf), transform(q)(pf))
      case FMap(p, f) => FMap(transform(p)(pf), f.simplify)
      case Many(p) => Many(transform(p)(pf))
      case Postfix(tpe, p, op) => Postfix(tpe, transform(p)(pf), transform(op)(pf))
      case LiftImplicit(f, ps) => LiftImplicit(f.simplify, ps.map(transform(_)(pf)))
      case LiftExplicit(f, ps) => LiftExplicit(f.simplify, ps.map(transform(_)(pf)))
      case Zipped(f, ps) => Zipped(f.simplify, ps.map(transform(_)(pf)))
      case Pure(f) => Pure(f.simplify)
      case _ => p
    }
  }

  implicit class ParserOps(private val p: Parser) extends AnyVal {
    def <*>(q: Parser): Parser = Ap(p, q).simplify
    def <|>(q: Parser): Parser = Choice(p, q).simplify
    def map(f: Function): Parser = FMap(p, f).simplify
  }
}
