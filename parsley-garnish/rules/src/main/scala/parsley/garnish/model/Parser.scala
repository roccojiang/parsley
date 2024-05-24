package parsley.garnish.model

import scala.PartialFunction.cond
import scala.meta._
import scalafix.v1._

import Function._
import parsley.garnish.implicits.TermOps

sealed abstract class Parser extends Product with Serializable {
  import Parser._

  def term: Term

  def normalise: Parser = this.rewrite {
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

    // empty.map(f) == empty  [proof in report appendix]
    case FMap(Empty, _) => Empty
    // pure(x).map(f) == pure(f) <*> pure(x) == pure(f(x))
    case FMap(Pure(x), f) => Pure(App(f, x))
    // p.map(f).map(g) == p.map(g compose f)
    case FMap(FMap(p, f), g) => FMap(p, composeH(g, f))

  }.simplifyFunctions

  private def simplifyFunctions: Parser = this.transform {
    case Pure(f) => Pure(f.normalise)
    case FMap(p, f) => FMap(p, f.normalise)
    case LiftImplicit(func, parsers) => LiftImplicit(func.normalise, parsers)
    case LiftExplicit(func, parsers) => LiftExplicit(func.normalise, parsers)
    case Zipped(func, parsers) => Zipped(func.normalise, parsers)
    case Bridge(func, parsers) => Bridge(func.normalise, parsers)
  }

  // Bottom-up transformation
  private def transform(pf: PartialFunction[Parser, Parser]): Parser = {
    val p = this match {
      case Choice(p, q) => Choice(p.transform(pf), q.transform(pf))
      case Ap(p, q) => Ap(p.transform(pf), q.transform(pf))
      case FMap(p, f) => FMap(p.transform(pf), f)
      case Many(p) => Many(p.transform(pf))
      case SomeP(p) => SomeP(p.transform(pf))
      case Postfix(tpe, p, op) => Postfix(tpe, p.transform(pf), op.transform(pf))
      case LiftImplicit(f, ps) => LiftImplicit(f, ps.map(_.transform(pf)))
      case LiftExplicit(f, ps) => LiftExplicit(f, ps.map(_.transform(pf)))
      case Zipped(f, ps) => Zipped(f, ps.map(_.transform(pf)))
      case Bridge(f, ps) => Bridge(f, ps.map(_.transform(pf)))
      case Pure(f) => Pure(f)
      case _ => this
    }
  
    if (pf.isDefinedAt(p)) pf(p) else p
  }

  // Transformation to normal form in a bottom-up manner
  private def rewrite(pf: PartialFunction[Parser, Parser]): Parser = {
    def pf0(p: Parser) = if (pf.isDefinedAt(p)) pf(p).rewrite(pf) else p

    this.transform(pf0)
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
  object Pure {
    val matcher = SymbolMatcher.normalized("parsley.Parsley.pure")

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, Pure] = {
      case Term.Apply.After_4_6_0(matcher(_), Term.ArgClause(List(x), _)) =>
        Pure(Function.buildFuncFromTerm(x, "PURE"))
    }
  }

  final case object Empty extends Parser {
    val term = Term.Name("empty")
    
    val matcher = SymbolMatcher.normalized("parsley.Parsley.empty")

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, Empty.type] = {
      case matcher(Term.Name(_)) => Empty
    }
  }

  /* p: Parser[A], q: Parser[A], p <|> q: Parser[A] */
  final case class Choice(p: Parser, q: Parser) extends Parser {
    // Note: for simplicity this doesn't preserve the original combinator choice (i.e. <|> or |)
    val term = q"${p.term} | ${q.term}"
  }
  object Choice {
    val matcher = SymbolMatcher.normalized("parsley.Parsley.`|`", "parsley.Parsley.`<|>`")

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, Choice] = {
      case Term.ApplyInfix.After_4_6_0(p, matcher(_), _, Term.ArgClause(List(q), _)) =>
        Choice(p.toParser, q.toParser)
    }
  }

  /* p: Parser[A => B], q: Parser[A], p <*> q: Parser[B] */
  final case class Ap(p: Parser, q: Parser) extends Parser {
    val term = q"${p.term} <*> ${q.term}"
  }
  object Ap {
    val matcher = SymbolMatcher.normalized("parsley.Parsley.`<*>`")

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, Ap] = {
      case Term.ApplyInfix.After_4_6_0(p, matcher(_), _, Term.ArgClause(List(q), _)) =>
        Ap(p.toParser, q.toParser)
    }
  }

  /* p: Parser[A], f: Func[A => B], p.map(f): Parser[B] */
  final case class FMap(p: Parser, f: Function) extends Parser {
    val term = q"${p.term}.map(${f.term})"
  }
  object FMap {
    val matcher = SymbolMatcher.normalized("parsley.Parsley.map")

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, FMap] = {
      case Term.Apply.After_4_6_0(Term.Select(qual, matcher(_)), Term.ArgClause(List(f), _)) =>
        FMap(qual.toParser, Function.buildFuncFromTerm(f, "MAP"))
    }
  }

  /* p: Parser[A], many(p): Parser[List[A]] */
  final case class Many(p: Parser) extends Parser {
    val term = q"many(${p.term})"
  }
  object Many {
    val matcher = SymbolMatcher.normalized("parsley.Parsley.many")

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, Many] = {
      case Term.Apply.After_4_6_0(matcher(_), Term.ArgClause(List(p), _)) =>
        Many(p.toParser)
    }
  }

  /* p: Parser[A], some(p): Parser[List[A]] */
  final case class SomeP(p: Parser) extends Parser {
    val term = q"some(${p.term})"
  }
  object SomeP {
    val matcher = SymbolMatcher.normalized("parsley.Parsley.some")

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, SomeP] = {
      case Term.Apply.After_4_6_0(matcher(_), Term.ArgClause(List(p), _)) =>
        SomeP(p.toParser)
    }
  }

  /* string(s): Parser[String] */
  final case class Str(s: String, implicitSyntax: Boolean = false) extends Parser {
    val term = if (implicitSyntax) Lit.String(s) else q"string($s)"
  }
  object Str {
    val stringLiftMatcher = SymbolMatcher.normalized("parsley.syntax.character.stringLift")
    val matcher = SymbolMatcher.normalized("parsley.character.string")

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, Str] = {
      case Term.Apply.After_4_6_0(matcher(_), Term.ArgClause(List(Lit.String(str)), _)) =>
        Str(str, implicitSyntax = false)

      case s @ Lit.String(str) if s.synthetics.exists(cond(_) {
        case ApplyTree(IdTree(symInfo), _) => stringLiftMatcher.matches(symInfo.symbol)
      }) => Str(str, implicitSyntax = true)
    }
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
  object LiftImplicit {
    val matcher = SymbolMatcher.normalized(
      (0 to 22).map(i => s"parsley.syntax.lift.Lift$i#lift"): _*
    )

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, LiftImplicit] = {
      case Term.Apply.After_4_6_0(Term.Select(f, matcher(_)), Term.ArgClause(ps, _)) =>
        val func = Function.buildFuncFromTerm(f match {
          case Term.ApplyType.After_4_6_0(g, _) => g
          case _ => f
        }, "LIFT_IMPLICIT")
        LiftImplicit(func, ps.map(_.toParser))
    }
  }

  final case class LiftExplicit(func: Function, parsers: List[Parser]) extends LiftLike {
    val term = {
      val liftN = Term.Name(s"lift${parsers.length}")
      q"$liftN(${func.term}, ..${parsers.map(_.term)})"
    }
  }
  object LiftExplicit {
    val matcher = SymbolMatcher.normalized(
      (1 to 22).map(i => s"parsley.lift.lift$i"): _*
    )

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, LiftExplicit] = {
      case Term.Apply.After_4_6_0(matcher(_), Term.ArgClause(f :: ps, _)) =>
        val func = Function.buildFuncFromTerm(f, "LIFT_EXPLICIT")
        LiftExplicit(func, ps.map(_.toParser))
    }
  }

  final case class Zipped(func: Function, parsers: List[Parser]) extends LiftLike {
    val term = q"(..${parsers.map(_.term)}).zipped(${func.term})"
  }
  object Zipped {
    val matcher = SymbolMatcher.normalized(
      (2 to 22).map(i => s"parsley.syntax.zipped.Zipped$i#zipped"): _*
    )

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, Zipped] = {
      case Term.Apply.After_4_6_0(Term.Select(Term.Tuple(ps), matcher(_)), Term.ArgClause(List(f), _)) =>
        val func = Function.buildFuncFromTerm(f, "ZIPPED")
        Zipped(func, ps.map(_.toParser))
    }
  }

  final case class Bridge(func: Function, parsers: List[Parser]) extends LiftLike {
    val term = q"${func.term}(..${parsers.map(_.term)})"
  }
  object Bridge {
    val matcher = SymbolMatcher.normalized(
      (1 to 22).map(i => s"parsley.generic.ParserBridge$i#apply"): _*
    )

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, Bridge] = {
      case Term.Apply.After_4_6_0(fun, ps) if fun.synthetics.exists(cond(_) {
          case SelectTree(_, IdTree(symInfo)) => matcher.matches(symInfo.symbol)
      }) =>
        val func = Function.buildFuncFromTerm(fun, "BRIDGE")
        Bridge(func, ps.map(_.toParser)) // TODO: I haven't unpacked the ArgClause here and directly mapped, does this work?
      }
  }

  final case class Named(name: String, parser: Parser) extends Parser {
    val term = q"$name(${parser.term})"
  }

  final case class Unknown(unrecognisedTerm: Term) extends Parser {
    val term = unrecognisedTerm
  }

  implicit class ParserOps(private val p: Parser) extends AnyVal {
    def <*>(q: Parser): Parser = Ap(p, q)
    def <|>(q: Parser): Parser = Choice(p, q)
    def map(f: Function): Parser = FMap(p, f)
  }
}
