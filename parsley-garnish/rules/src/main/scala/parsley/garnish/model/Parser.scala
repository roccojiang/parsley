package parsley.garnish.model

import scala.PartialFunction.cond
import scala.meta._
import scalafix.v1._

import Expr._
import parsley.garnish.implicits.TermOps
import parsley.garnish.analysis.ParserTransformer.ParserDefinition

sealed abstract class Parser extends Product with Serializable {
  import Parser._

  def term: Term

  def unfold(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser

  def isEquivalent(other: Parser): Boolean = this.normalise == other.normalise

  /* Faster(?) than prettification, used for equivalence checking */
  def normalise: Parser = this.rewrite { case Tag(_, p) => p }.simplify.normaliseExprs

  /* Simplify parsers and attempt to resugar them */
  // def prettify: Parser = this.simplify.resugar.simplify.normaliseExprs
  def prettify = normalise.resugar

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
    // TODO: Targeted resugaring based on desugaring tags
    case Tag(_, parser) => parser
      // parser.transform(resugarer)

  }.rewrite {
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

  def normaliseExprs: Parser = this.transform {
    case Pure(f) => Pure(f.normalise)
    case FMap(p, f) => FMap(p, f.normalise)
    case LiftImplicit(func, parsers) => LiftImplicit(func.normalise, parsers)
    case LiftExplicit(func, parsers) => LiftExplicit(func.normalise, parsers)
    case Zipped(func, parsers) => Zipped(func.normalise, parsers)
    case Bridge(func, parsers) => Bridge(func.normalise, parsers)
  }

  // Bottom-up transformation
  def transform(pf: PartialFunction[Parser, Parser]): Parser = {
    val parser = this match {
      case p <|> q => <|>(p.transform(pf), q.transform(pf))
      case p <*> q => <*>(p.transform(pf), q.transform(pf))
      case p ~> q  => ~>(p.transform(pf), q.transform(pf))
      case p <~ q  => <~(p.transform(pf), q.transform(pf))
      case FMap(p, f) => FMap(p.transform(pf), f)
      case ManyP(p) => ManyP(p.transform(pf))
      case SomeP(p) => SomeP(p.transform(pf))
      case Postfix(tpe, p, op) => Postfix(tpe, p.transform(pf), op.transform(pf))
      case LiftImplicit(f, ps) => LiftImplicit(f, ps.map(_.transform(pf)))
      case LiftExplicit(f, ps) => LiftExplicit(f, ps.map(_.transform(pf)))
      case Zipped(f, ps) => Zipped(f, ps.map(_.transform(pf)))
      case Bridge(f, ps) => Bridge(f, ps.map(_.transform(pf)))
      case EndBy(p, sep) => EndBy(p.transform(pf), sep.transform(pf))

      case Tag(resugarer, p) => Tag(resugarer, p.transform(pf))

      case s: Str => s
      case c: Chr => c
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

  // override def toString: String = term.syntax
}

object Parser {
  type Grammar = Map[Symbol, ParserDefinition]

  case class UnfoldingContext(visited: Set[Symbol], env: Grammar, nonTerminal: Symbol)
  case class UnfoldedParser(results: Option[Expr], nonLeftRec: Parser, leftRec: Parser) {
    val isLeftRecursive = leftRec.normalise != Empty
  }

  final case class Tag(resugarer: PartialFunction[Parser, Parser], parser: Parser) extends Parser {
    def term = q"TAGGED(${parser.term})"

    override def unfold(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser = {
      val UnfoldedParser(results, nonLeftRec, leftRec) = parser.unfold
      UnfoldedParser(results, Tag(resugarer, nonLeftRec), Tag(resugarer, leftRec))
    }
  }

  final case class NonTerminal(ref: Symbol)(implicit doc: SemanticDocument) extends Parser {
    val term = Term.Name(ref.info.get.displayName)

    override def unfold(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser = {
      assert(ctx.env.contains(ref), s"expected to find non-terminal $ref in this file, instead found: ${ctx.env.keys}")

      // val tpe = getParsleyType(ref)
      // assert(tpe.isDefined, s"expected a Parsley type for $ref, got ${ref.info.get.signature}")

      if (ref == ctx.nonTerminal) {
        UnfoldedParser(None, Empty, Pure(id))
      } else if (ctx.visited.contains(ref)) {
        UnfoldedParser(None, NonTerminal(ref), Empty)
      } else {
        val unfoldedRef = ctx.env(ref).parser.unfold(ctx.copy(visited = ctx.visited + ref), doc)

        // println(s"UNFOLDEDREF $ref: ${unfoldedRef.results} ### ${unfoldedRef.nonLeftRec.prettify} ### ${unfoldedRef.leftRec.prettify}")

        if (unfoldedRef.results.isDefined) {
          unfoldedRef
          // UnfoldedParser(None, NonTerminal(ref), Empty)
        } else if (!unfoldedRef.isLeftRecursive) {
          // the non-terminal we recursively unfolded was not left-recursive, so we just reference its name directly,
          // rather than aggressively inlining it
          UnfoldedParser(None, NonTerminal(ref), Empty)
        } else {
          // in the left-recursive case, we must inline the result of the unfolded non-terminal
          unfoldedRef
        }
      }
    }
  }

  /* x: A, pure(x): Parser[A] */
  final case class Pure(x: Expr) extends Parser {
    val term = q"pure(${x.term})"

    override def unfold(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser = {
      UnfoldedParser(Some(x), Empty, Empty)
    }
  }
  object Pure {
    val matcher = SymbolMatcher.normalized("parsley.ParsleyImpl.pure")

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, Pure] = {
      case Term.Apply.After_4_6_0(matcher(_), Term.ArgClause(List(func), _)) =>
        Pure(func.toExpr("PURE"))
    }
  }

  final case object Empty extends Parser {
    val term = Term.Name("empty")

    override def unfold(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser = {
      UnfoldedParser(None, Empty, Empty)
    }

    val matcher = SymbolMatcher.normalized("parsley.ParsleyImpl.empty")

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, Empty.type] = {
      case matcher(Term.Name(_)) => Empty
    }
  }

  /* p: Parser[A], q: Parser[A], p <|> q: Parser[A] */
  final case class <|>(p: Parser, q: Parser) extends Parser {
    // Note: for simplicity this doesn't preserve the original combinator choice (i.e. <|> or |)
    val term = q"${p.term} | ${q.term}"

    override def unfold(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser = {
      val UnfoldedParser(pe, pn, pl) = p.unfold
      val UnfoldedParser(qe, qn, ql) = q.unfold

      UnfoldedParser(pe.orElse(qe), pn | qn, pl | ql)
    }
  }
  object <|> {
    val matcher = SymbolMatcher.normalized("parsley.Parsley.`|`", "parsley.Parsley.`<|>`")

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, <|>] = {
      case Term.ApplyInfix.After_4_6_0(p, matcher(_), _, Term.ArgClause(List(q), _)) =>
        <|>(p.toParser, q.toParser)
    }
  }

  /* p: Parser[A => B], q: Parser[A], p <*> q: Parser[B] */
  final case class <*>(p: Parser, q: Parser) extends Parser {
    val term = q"${p.term} <*> ${q.term}"

    override def unfold(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser = {
      val UnfoldedParser(pe, pn, pl) = p.unfold
      val UnfoldedParser(qe, qn, ql) = q.unfold

      val result =
        if (pe.isDefined && qe.isDefined) Some(App(pe.get, qe.get)) // pure f <*> pure x = pure (f x)
        else None

      val lefts = {
        val llr = pl.map(flip) <*> q
        val rlr = pe.map(f => ql.map(compose(f))).getOrElse(Empty)
        llr | rlr
      }

      val nonLefts = {
        val lnl = pn <*> q
        val rnl = pe.map(f => qn.map(f)).getOrElse(Empty)
        lnl | rnl
      }

      UnfoldedParser(result, nonLefts, lefts)
    }
  }
  object <*> {
    val matcher = SymbolMatcher.normalized("parsley.Parsley.`<*>`")

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, <*>] = {
      case Term.ApplyInfix.After_4_6_0(p, matcher(_), _, Term.ArgClause(List(q), _)) =>
        <*>(p.toParser, q.toParser)
    }
  }

  final case class ~>(p: Parser, q: Parser) extends Parser {
    val term = q"${p.term} ~> ${q.term}"

    override def unfold(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser = {
      val x = Var.fresh()
      val y = Var.fresh()
      // const id = \x -> \y -> y
      val f = Abs(x, Abs(y, y))

      Tag(resugaring.thenResugarer, p.map(f) <*> q).unfold
    }
  }
  object ~> {
    val matcher = SymbolMatcher.normalized("parsley.Parsley.`~>`", "parsley.Parsley.`*>`")

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, ~>] = {
      case Term.ApplyInfix.After_4_6_0(p, matcher(_), _, Term.ArgClause(List(q), _)) =>
        ~>(p.toParser, q.toParser)
    }
  }

  final case class <~(p: Parser, q: Parser) extends Parser {
    val term = q"${p.term} <~ ${q.term}"

    override def unfold(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser = {
      val x = Var.fresh()
      val y = Var.fresh()
      // const = \x -> \y -> x
      val f = Abs(x, Abs(y, x))

      (p.map(f) <*> q).unfold
    }
  }
  object <~ {
    val matcher = SymbolMatcher.normalized("parsley.Parsley.`<~`", "parsley.Parsley.`<*`")

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, <~] = {
      case Term.ApplyInfix.After_4_6_0(p, matcher(_), _, Term.ArgClause(List(q), _)) =>
        <~(p.toParser, q.toParser)
    }
  }

  /* p: Parser[A], f: Func[A => B], p.map(f): Parser[B] */
  final case class FMap(p: Parser, f: Expr) extends Parser {
    val term = q"${p.term}.map(${f.term})"

    override def unfold(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser = {
      (Pure(f) <*> p).unfold
    }
  }
    object FMap {
    val matcher = SymbolMatcher.normalized("parsley.Parsley.map")

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, FMap] = {
      case Term.Apply.After_4_6_0(Term.Select(qual, matcher(_)), Term.ArgClause(List(func), _)) =>
        FMap(qual.toParser, func.toExpr("MAP"))
    }
  }

  /* p: Parser[A], many(p): Parser[List[A]] */
  final case class ManyP(p: Parser) extends Parser {
    val term = q"many(${p.term})"

    override def unfold(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser = {
      val UnfoldedParser(_, pn, pl) = p.unfold

      val lefts = pl.map {
        val f = Var.fresh()
        val xs = Var.fresh()
        val nt = Var.fresh()

        // \f xs nt -> f nt : xs
        Abs(f, Abs(xs, Abs(nt, cons(App(f, nt), xs))))
      } <*> ManyP(p)

      val nonLefts = SomeP(pn)

      UnfoldedParser(Some(Translucent(q"Nil")), nonLefts, lefts)
    }
  }
  object ManyP {
    val matcher = SymbolMatcher.normalized("parsley.ParsleyImpl.many")

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, ManyP] = {
      case Term.Apply.After_4_6_0(matcher(_), Term.ArgClause(List(p), _)) =>
        ManyP(p.toParser)
    }
  }

  /* p: Parser[A], some(p): Parser[List[A]] */
  final case class SomeP(p: Parser) extends Parser {
    val term = q"some(${p.term})"

    override def unfold(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser = {
      (p.map(cons) <*> ManyP(p)).unfold
    }
  }
  object SomeP {
    val matcher = SymbolMatcher.normalized("parsley.ParsleyImpl.some")

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, SomeP] = {
      case Term.Apply.After_4_6_0(matcher(_), Term.ArgClause(List(p), _)) =>
        SomeP(p.toParser)
    }
  }

  /* string(s): Parser[String] */
  final case class Str(s: String, implicitSyntax: Boolean = false) extends Parser {
    val term = if (implicitSyntax) Lit.String(s) else q"string($s)"

    override def unfold(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser =
      UnfoldedParser(None, this, Empty)
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

  /* char(s): Parser[Char] */
  final case class Chr(c: Char, implicitSyntax: Boolean = false) extends Parser {
    val term = if (implicitSyntax) Lit.Char(c) else q"char($c)"

    override def unfold(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser =
      UnfoldedParser(None, this, Empty)
  }
  object Chr {
    val charLiftMatcher = SymbolMatcher.normalized("parsley.syntax.character.charLift")
    val matcher = SymbolMatcher.normalized("parsley.character.char")

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, Chr] = {
      case Term.Apply.After_4_6_0(matcher(_), Term.ArgClause(List(Lit.Char(chr)), _)) =>
        Chr(chr, implicitSyntax = false)

      case s @ Lit.Char(chr) if s.synthetics.exists(cond(_) {
        case ApplyTree(IdTree(symInfo), _) => charLiftMatcher.matches(symInfo.symbol)
      }) => Chr(chr, implicitSyntax = true)
    }
  }

  /* p: Parser[A], op: Parser[A => A], p postfix op: Parser[A] */
  final case class Postfix(tpe: Type.Name, p: Parser, op: Parser) extends Parser {
    val term = q"chain.postfix[$tpe](${p.term})(${op.term})"

    override def unfold(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser = {
      UnfoldedParser(None, this, Empty) // TODO: actually implement properly
    }
  }

  /* f: (T1, T2, ..., TN) => R, pN: Parser[TN], f.lift(ps) : Parser[R] */
  sealed trait LiftLike extends Parser {
    def func: Expr
    def parsers: List[Parser]

    override def unfold(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser = {
      val liftedFunc: Parser = Pure(func match {
        // The dodgy case: had to treat the entire function as opaque
        case Translucent_(f, substs) if parsers.size > 1 => Translucent(f, substs, isCurried = true)
        // The normal case: this function should've been lifted to Expr correctly, so currying actually works normally
        case _ => func.curried
      })

      val curriedForm = this match {
        case _: Bridge => Tag(resugaring.bridgeApply, parsers.foldLeft(liftedFunc)(_ <*> _))
        case _ => parsers.foldLeft(liftedFunc)(_ <*> _)
      }
      // println(s"CURRIED>>> ${curriedForm.term.syntax}")

      curriedForm.unfold
    }
  }

  final case class LiftImplicit(func: Expr, parsers: List[Parser]) extends LiftLike {
    val term = q"${func.term}.lift(..${parsers.map(_.term)})"
  }
  object LiftImplicit {
    val matcher = SymbolMatcher.normalized(
      (0 to 22).map(i =>
        Seq(s"parsley.syntax.Lift$i#lift", s"parsley.syntax.lift.liftSyntax$i")
      ).flatten: _*
    )

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, LiftImplicit] = {
      case Term.Apply.After_4_6_0(Term.Select(f, matcher(_)), Term.ArgClause(ps, _)) =>
        val func = f match {
          case Term.ApplyType.After_4_6_0(g, _) => g
          case _ => f
        }
        LiftImplicit(func.toExpr("LIFT_IMPLICIT", Some(ps.size)), ps.map(_.toParser))
    }
  }

  final case class LiftExplicit(func: Expr, parsers: List[Parser]) extends LiftLike {
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
        LiftExplicit(f.toExpr("LIFT_EXPLICIT", Some(ps.size)), ps.map(_.toParser))
    }
  }

  final case class Zipped(func: Expr, parsers: List[Parser]) extends LiftLike {
    val term = q"(..${parsers.map(_.term)}).zipped(${func.term})"
  }
  object Zipped {
    val matcher = SymbolMatcher.normalized(
      (2 to 22).map(i =>
        Seq(s"parsley.syntax.Zipped$i#zipped", s"parsley.syntax.zipped.zippedSyntax$i")
      ).flatten: _*
    )

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, Zipped] = {
      case Term.Apply.After_4_6_0(Term.Select(Term.Tuple(ps), matcher(_)), Term.ArgClause(List(f), _)) =>
        Zipped(f.toExpr("ZIPPED", Some(ps.size)), ps.map(_.toParser))
    }
  }

  final case class Bridge(func: Expr, parsers: List[Parser]) extends LiftLike {
    val term = q"${func.term}(..${parsers.map(_.term)})"
  }
  object Bridge {
    val matcher = SymbolMatcher.normalized(
      (1 to 22).map(i => s"parsley.generic.ParserBridge$i#apply"): _*
    )

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, Bridge] = {
      case Term.Apply.After_4_6_0(func, ps) if func.synthetics.exists(cond(_) {
          case SelectTree(_, IdTree(symInfo)) => matcher.matches(symInfo.symbol)
      }) =>
        Bridge(func.toExpr("BRIDGE", Some(ps.size)), ps.map(_.toParser)) // directly mapping over the ArgClause without unpacking it seems to work fine
      }
  }

  final case class EndBy(p: Parser, sep: Parser) extends Parser {
    val term = q"endBy(${p.term}, ${sep.term})"

    override def unfold(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser = ManyP(p <~ sep).unfold
  }
  object EndBy {
    val matcher = SymbolMatcher.normalized("parsley.combinator.endBy")

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, EndBy] = {
      case Term.Apply.After_4_6_0(matcher(_), Term.ArgClause(List(p, sep), None)) =>
        EndBy(p.toParser, sep.toParser)
    }
  }

  final case class Unknown(unrecognisedTerm: Term) extends Parser {
    val term = unrecognisedTerm

    override def unfold(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser = {
      UnfoldedParser(None, this, Empty)
    }
  }

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
