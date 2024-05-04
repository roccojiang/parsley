package fix.utils

import scala.meta._
import scalafix.v1._

import Func._
import NonTerminalDetection.NonTerminalTree
import TypeUtils._

sealed abstract class Parser extends Product with Serializable {
  import Parser._
  
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
      case Choice(p, Empty)   => p.simplify
      case Choice(Empty, q)   => q.simplify
      case Choice(Pure(f), _) => Pure(f.simplify)
      case Choice(p, q)       => Choice(p.simplify, q.simplify)

      case Ap(Empty, _)         => Empty
      case Ap(Pure(f), Pure(x)) => Pure(App(f.simplify, x.simplify).simplify)
      case Ap(Pure(f), x)       => FMap(x.simplify, f.simplify)
      case Ap(p, q)             => Ap(p.simplify, q.simplify)

      case p @ FMap(_, _) => simplifyMap(p)

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

  // TODO: collapse these back down into a single case class
  sealed trait LiftN extends Parser
  /* f: Func[A => B => R], p1: Parser[A], p2: Parser[B], lift2(f, p1, p2): Parser[R] */
  final case class Lift2(f: Func, p1: Parser, p2: Parser, isImplicit: Boolean) extends LiftN {
    val term = if (isImplicit) {
      q"${f.term}.lift(${p1.term}, ${p2.term})"
    } else {
      q"lift2(${f.term}, ${p1.term}, ${p2.term})"
    }
  }
  final case class Lift3(f: Func, p1: Parser, p2: Parser, p3: Parser, isImplicit: Boolean) extends LiftN {
    val term = if (isImplicit) {
      q"${f.term}.lift(${p1.term}, ${p2.term}, ${p3.term})"
    } else {
      q"lift3(${f.term}, ${p1.term}, ${p2.term}, ${p3.term})"
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
    case Term.Apply.After_4_6_0(Matchers.pure(_), Term.ArgClause(List(x), _)) => Pure(UserDefined(x))
    // "p.map(f)"
    case Term.Apply.After_4_6_0(Term.Select(qual, Matchers.map(_)), Term.ArgClause(List(f), _)) => {
      println(s"MAP ${f.structure}")

      val lambda = f.collect {
        case Term.Apply.After_4_6_0(g: Term.Name, _) => {
          // println(s"$g symbol sig ${g.symbol.info.map(i => i.signature.structure)}") // this gets a class signature, it seems
          println(s"${g.structure} func within map: ${g.synthetics} | ${g.synthetics.structure} )}") // TODO: this seems to find the correct Term.Names
          val typeSignature = collectInferredType(g.synthetics)
          println(s"typeSignature: $typeSignature")
          val lambdaTerm = Func.fromTypeSignature(UserDefined(g), typeSignature)
          pprint.pprintln(lambdaTerm)
          lambdaTerm
        }
      }.headOption

      assert(lambda.isDefined)
      FMap(apply(qual), UserDefined(f))
    }
    // "p <|> q" or "p | q"
    case Term.ApplyInfix.After_4_6_0(p, Matchers.<|>(_), _, Term.ArgClause(List(q), _)) => Choice(apply(p), apply(q))
    // "p <*> q"
    case Term.ApplyInfix.After_4_6_0(p, Matchers.<*>(_), _, Term.ArgClause(List(q), _)) => Ap(apply(p), apply(q))

    // "liftN(f, p1, ..., pN)"
    case Term.Apply.After_4_6_0(Matchers.liftExplicit(_), Term.ArgClause(f :: ps, _)) if (ps.length == 2) => {
      // println(s"${f.structure} in lift2: ${f.synthetics} | ${f.synthetics.structure}") // TODO: this doesn't contain the needed info
      f collect {
        case Term.Apply.After_4_6_0(g, _) => {
          println(s"${g.structure} func within lift: ${g.synthetics} | ${g.synthetics.structure}") // TODO: this finds the Term.Name node with needed synthetics
          val typeSignature = collectInferredType(g.synthetics)
          println(s"typeSignature: $typeSignature")
        }
      }
      Lift2(UserDefined(f), apply(ps(0)), apply(ps(1)), isImplicit = false)
    }
    case Term.Apply.After_4_6_0(Matchers.liftExplicit(_), Term.ArgClause(f :: ps, _)) if (ps.length == 3) => {
      println(s"$f in lift3: ${f.synthetics} | ${f.synthetics.structure}")
      Lift3(UserDefined(f), apply(ps(0)), apply(ps(1)), apply(ps(2)), isImplicit = false)
    }

    // "f.lift(p1, ..., pN)"
    case Term.Apply.After_4_6_0(Term.Select(f, Matchers.liftImplicit(_)), Term.ArgClause(ps, _)) if (ps.length == 2) => {
      println(s"${f.structure} in lift2: ${f.synthetics} | ${f.synthetics.structure}") // TODO: this contains the needed synthetics
      // f collect {
      //   case Term.Apply.After_4_6_0(g, _) => println(s"$g func within lift: ${g.synthetics} | ${g.synthetics.structure}") // TODO: this doesn't exist, since f is a Term.Name
      // }
      Lift2(UserDefined(f), apply(ps(0)), apply(ps(1)), isImplicit = true)
    }
    case Term.Apply.After_4_6_0(Term.Select(f, Matchers.liftImplicit(_)), Term.ArgClause(ps, _)) if (ps.length == 3) => {
      println(s"$f in lift3: ${f.synthetics} | ${f.synthetics.structure}")
      Lift3(UserDefined(f), apply(ps(0)), apply(ps(1)), apply(ps(2)), isImplicit = true)
    }

    // "(p1, ..., pN).zipped(f)"
    case p@Term.Apply.After_4_6_0(Term.Select(Term.Tuple(ps), Matchers.zipped(_)), Term.ArgClause(List(f), _)) => {
      println(s"ZIPPED ${p.structure}")

      val lambda = f.collect {
        case Term.Apply.After_4_6_0(g: Term.Name, _) => {
          println(s"${g.structure} func within zipped: ${g.synthetics} | ${g.synthetics.structure} )}") // TODO: this seems to find the correct Term.Names
          val typeSignature = collectInferredType(g.synthetics)
          println(s"typeSignature: $typeSignature")
          val lambdaTerm = Func.fromTypeSignature(UserDefined(g), typeSignature)
          pprint.pprintln(lambdaTerm)
          lambdaTerm
        }
      }.headOption

      // TODO actually put the correct parser in
      Lift2(UserDefined(f), apply(ps(0)), apply(ps(1)), isImplicit = true)
    }

    // TODO: pattern match on Apply, ApplyInfix so we can still try to find parsers within the term?
    case unrecognisedTerm => Unknown(unrecognisedTerm)
  }

  implicit class ParserOps(p: Parser) {
    def <*>(q: Parser): Parser = Ap(p, q).simplify
    def <|>(q: Parser): Parser = Choice(p, q).simplify
    def map(f: Func): Parser = FMap(p, f).simplify
  }
}
