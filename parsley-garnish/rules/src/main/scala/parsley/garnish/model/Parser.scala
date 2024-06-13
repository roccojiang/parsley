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

    // f.curried.map(p) <*> q == (p, q).zipped(f)
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

    // Scala 2 cannot resolve implicit stringLift on "s".map(f)
    case FMap(Str(s, _), f) => FMap(Str(s, implicitSyntax = false), f)
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
      case Choice(p, q) => Choice(p.transform(pf), q.transform(pf))
      case Ap(p, q) => Ap(p.transform(pf), q.transform(pf))
      case Then(p, q) => Then(p.transform(pf), q.transform(pf))
      case ThenDiscard(p, q) => ThenDiscard(p.transform(pf), q.transform(pf))
      case FMap(p, f) => FMap(p.transform(pf), f)
      case Many(p) => Many(p.transform(pf))
      case SomeP(p) => SomeP(p.transform(pf))
      case Postfix(tpe, p, op) => Postfix(tpe, p.transform(pf), op.transform(pf))
      case LiftImplicit(f, ps) => LiftImplicit(f, ps.map(_.transform(pf)))
      case LiftExplicit(f, ps) => LiftExplicit(f, ps.map(_.transform(pf)))
      case Zipped(f, ps) => Zipped(f, ps.map(_.transform(pf)))
      case Bridge(f, ps) => Bridge(f, ps.map(_.transform(pf)))
      case EndBy(p, sep) => EndBy(p.transform(pf), sep.transform(pf))

      case Tag(resugarer, p) => Tag(resugarer, p.transform(pf))

      case s: Str => s
      case p: Pure => p
      case Empty => Empty
      case nt: NonTerminal => nt
      case unk: Unknown => unk
    }

    if (pf.isDefinedAt(parser)) pf(parser) else parser
  }

  // Transformation to normal form in a bottom-up manner
  def rewrite(pf: PartialFunction[Parser, Parser]): Parser = {
    def pf0(p: Parser) = if (pf.isDefinedAt(p)) pf(p).rewrite(pf) else p

    this.transform(pf0)
  }

  // override def toString: String = term.syntax
}

// TODO: https://github.com/j-mie6/parsley/issues/236 opaque-style tagging?
object Parser {

  case class UnfoldingContext(visited: Set[Symbol], env: Map[Symbol, ParserDefinition], nonTerminal: Symbol)
  case class UnfoldedParser(empty: Option[Expr], nonLeftRec: Parser, leftRec: Parser) {
    val isLeftRecursive = leftRec.normalise != Empty
  }

  final case class Tag(resugarer: PartialFunction[Parser, Parser], parser: Parser) extends Parser {
    def term = q"TAGGED(${parser.term})"

    override def unfold(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser = {
      val UnfoldedParser(empty, nonLeftRec, leftRec) = parser.unfold
      UnfoldedParser(empty, Tag(resugarer, nonLeftRec), Tag(resugarer, leftRec))
    }
  }

  final case class NonTerminal(ref: Symbol)(implicit doc: SemanticDocument) extends Parser {
    val term = Term.Name(ref.info.get.displayName)

    override def unfold(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser = {
      assert(ctx.env.contains(ref), s"expected to find non-terminal $ref in this file")

      // val tpe = getParsleyType(ref)
      // assert(tpe.isDefined, s"expected a Parsley type for $ref, got ${ref.info.get.signature}")

      if (ref == ctx.nonTerminal) {
        UnfoldedParser(None, Empty, Pure(id))
      } else if (ctx.visited.contains(ref)) {
        UnfoldedParser(None, NonTerminal(ref), Empty)
      } else {
        val unfoldedRef = ctx.env(ref).parser.unfold(ctx.copy(visited = ctx.visited + ref), doc)

        if (!unfoldedRef.isLeftRecursive) {
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
  final case class Choice(p: Parser, q: Parser) extends Parser {
    // Note: for simplicity this doesn't preserve the original combinator choice (i.e. <|> or |)
    val term = q"${p.term} | ${q.term}"

    override def unfold(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser = {
      val UnfoldedParser(pe, pn, pl) = p.unfold
      val UnfoldedParser(qe, qn, ql) = q.unfold

      // TODO: originally in the paper this was pe.xor(qe), but I think that's not true under PEG semantics?
      UnfoldedParser(pe.orElse(qe), pn <|> qn, pl <|> ql)
    }
  }
  object Choice {
    val matcher = SymbolMatcher.normalized("parsley.Parsley.`|`", "parsley.Parsley.`<|>`")

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, Choice] = {
      case Term.ApplyInfix.After_4_6_0(p, matcher(_), _, Term.ArgClause(List(q), _)) =>
        Choice(p.toParser, q.toParser)
    }
  }
  object <|> {
    def unapply(parser: Choice): Option[(Parser, Parser)] = Some((parser.p, parser.q))
  }

  /* p: Parser[A => B], q: Parser[A], p <*> q: Parser[B] */
  final case class Ap(p: Parser, q: Parser) extends Parser {
    val term = q"${p.term} <*> ${q.term}"

    override def unfold(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser = {
      val UnfoldedParser(pe, pn, pl) = p.unfold
      val UnfoldedParser(qe, qn, ql) = q.unfold

      val empty =
        if (pe.isDefined && qe.isDefined) Some(App(pe.get, qe.get)) // pure f <*> pure x = pure (f x)
        else None

      val lefts = {
        val llr = pl.map(flip) <*> q
        val rlr = pe.map(f => ql.map(compose(f))).getOrElse(Empty)
        llr <|> rlr
      }

      val nonLefts = {
        val lnl = pn <*> q
        val rnl = pe.map(f => qn.map(f)).getOrElse(Empty)
        lnl <|> rnl
      }

      UnfoldedParser(empty, nonLefts, lefts)
    }
  }
  object Ap {
    val matcher = SymbolMatcher.normalized("parsley.Parsley.`<*>`")

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, Ap] = {
      case Term.ApplyInfix.After_4_6_0(p, matcher(_), _, Term.ArgClause(List(q), _)) =>
        Ap(p.toParser, q.toParser)
    }
  }
  object <*> {
    def unapply(parser: Ap): Option[(Parser, Parser)] = Some((parser.p, parser.q))
  }

  final case class Then(p: Parser, q: Parser) extends Parser {
    val term = q"${p.term} ~> ${q.term}"

    override def unfold(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser = {
      val x = Var.fresh()
      val y = Var.fresh()
      // const id = \x -> \y -> y
      val f = Abs(x, Abs(y, y))

      Tag(resugaring.thenResugarer, p.map(f) <*> q).unfold
    }
  }
  object Then {
    val matcher = SymbolMatcher.normalized("parsley.Parsley.`~>`", "parsley.Parsley.`*>`")

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, Then] = {
      case Term.ApplyInfix.After_4_6_0(p, matcher(_), _, Term.ArgClause(List(q), _)) =>
        Then(p.toParser, q.toParser)
    }
  }
  object ~> {
    def unapply(parser: Then): Option[(Parser, Parser)] = Some((parser.p, parser.q))
  }

  final case class ThenDiscard(p: Parser, q: Parser) extends Parser {
    val term = q"${p.term} <~ ${q.term}"

    override def unfold(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser = {
      val x = Var.fresh()
      val y = Var.fresh()
      // const = \x -> \y -> x
      val f = Abs(x, Abs(y, x))

      (p.map(f) <*> q).unfold
    }
  }
  object ThenDiscard {
    val matcher = SymbolMatcher.normalized("parsley.Parsley.`<~`", "parsley.Parsley.`<*`")

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, ThenDiscard] = {
      case Term.ApplyInfix.After_4_6_0(p, matcher(_), _, Term.ArgClause(List(q), _)) =>
        ThenDiscard(p.toParser, q.toParser)
    }
  }
  object <~ {
    def unapply(parser: ThenDiscard): Option[(Parser, Parser)] = Some((parser.p, parser.q))
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
  final case class Many(p: Parser) extends Parser {
    val term = q"many(${p.term})"

    override def unfold(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser = {
      val UnfoldedParser(_, pn, pl) = p.unfold

      val lefts = pl.map {
        val f = Var.fresh()
        val xs = Var.fresh()
        val nt = Var.fresh()

        // \f xs nt -> f nt : xs
        Abs(f, Abs(xs, Abs(nt, cons(App(f, nt), xs))))
      } <*> Many(p)

      val nonLefts = SomeP(pn)

      UnfoldedParser(Some(Translucent(q"Nil")), nonLefts, lefts)
    }
  }
  object Many {
    val matcher = SymbolMatcher.normalized("parsley.ParsleyImpl.many")

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, Many] = {
      case Term.Apply.After_4_6_0(matcher(_), Term.ArgClause(List(p), _)) =>
        Many(p.toParser)
    }
  }

  /* p: Parser[A], some(p): Parser[List[A]] */
  final case class SomeP(p: Parser) extends Parser {
    val term = q"some(${p.term})"

    override def unfold(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser = {
      (p.map(cons) <*> Many(p)).unfold
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

    override def unfold(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser = {
      UnfoldedParser(None, this, Empty)
    }
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

    override def unfold(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser = {
      UnfoldedParser(None, this, Empty) // TODO: actually implement properly
    }
  }

  /* f: (T1, T2, ..., TN) => R, pN: Parser[TN], f.lift(ps) : Parser[R] */
  sealed trait LiftLike extends Parser {
    def func: Expr
    def parsers: List[Parser]

    override def unfold(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser = {
      // TODO: properly keep track if things were curried
      val liftedFunc: Parser = Pure(func match {
        case Translucent(f @ Term.Name(_), substs) if parsers.size > 1 => Translucent(q"$f.curried", substs)
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
        LiftImplicit(func.toExpr("LIFT_IMPLICIT"), ps.map(_.toParser))
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
        LiftExplicit(f.toExpr("LIFT_EXPLICIT"), ps.map(_.toParser))
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
        Zipped(f.toExpr("ZIPPED"), ps.map(_.toParser))
    }
  }

  final case class Bridge(func: Expr, parsers: List[Parser]) extends LiftLike {
    // val term = q"${func.term}(..${parsers.map(_.term)})"
    val term = AppN(func, parsers.map(p => Translucent(p.term))).normalise.term
  }
  object Bridge {
    val matcher = SymbolMatcher.normalized(
      (1 to 22).map(i => s"parsley.generic.ParserBridge$i#apply"): _*
    )

    def fromTerm(implicit doc: SemanticDocument): PartialFunction[Term, Bridge] = {
      case Term.Apply.After_4_6_0(func, ps) if func.synthetics.exists(cond(_) {
          case SelectTree(_, IdTree(symInfo)) => matcher.matches(symInfo.symbol)
      }) =>
        Bridge(func.toExpr("BRIDGE"), ps.map(_.toParser)) // directly mapping over the ArgClause without unpacking it seems to work fine
      }
  }

  final case class EndBy(p: Parser, sep: Parser) extends Parser {
    val term = q"endBy(${p.term}, ${sep.term})"

    override def unfold(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser = Many(p <~ sep).unfold
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
    def <*>(q: Parser): Parser = Ap(p, q)
    def ~>(q: Parser): Parser = Then(p, q)
    def <~(q: Parser): Parser = ThenDiscard(p, q)
    def <|>(q: Parser): Parser = Choice(p, q)
    def map(f: Expr): Parser = FMap(p, f)
  }

  implicit class MultiParserOps(private val ps: List[Parser]) extends AnyVal {
    def zipped(f: Expr): Parser = Zipped(f, ps)
  }
}
