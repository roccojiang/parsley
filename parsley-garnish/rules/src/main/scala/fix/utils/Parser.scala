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

  final case class Unknown(unrecognisedTerm: Term) extends Parser {
    val term = unrecognisedTerm
  }

  def apply(term: Term)(implicit env: Map[Symbol, NonTerminalTree], doc: SemanticDocument): Parser = term match {
    // See https://scalacenter.github.io/scalafix/docs/developers/symbol-matcher.html#unapplytree for how to mitigate
    // against matching multiple times using SymbolMatchers

    case t if env.contains(t.symbol) => NonTerminal(t.symbol)

    // "string(s)"
    case Term.Apply.After_4_6_0(Matchers.string(_), Term.ArgClause(List(s), _)) => Str(s.toString)
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
      // case (Empty, _) => Empty // TODO: does this hold?
      case (p, f)     => FMap(p, f)
    }
  }
}


// sealed abstract class Parser extends Product with Serializable
// object Parser {
//   // TODO: for everything that uses Term arguments, should we use a different type? Perhaps some way to represent functions, for defunctionalisation?
//   final case class NonTerminal(ref: Symbol) extends Parser
//   final case class Pure(a: Term) extends Parser
//   final case object Empty extends Parser
//   /* p: Parser[A], q: Parser[A] */
//   final case class Or(p: Parser, q: Parser) extends Parser
//   // TODO: generalise to lift2?
//   /* p: Parser[A => B], q: Parser[A] */
//   final case class <*>(p: Parser, q: Parser) extends Parser
//   /* p: Parser[A], f: A => B */
//   final case class FMap(p: Parser, f: Term) extends Parser
//   final case class Many(p: Parser) extends Parser
//   final case class Str(str: String) extends Parser

//   final case object Unknown extends Parser
// }

// // The original term this parser is connected to. If None, then this parser is a result of a transformation
// // TODO: better naming?
// sealed abstract class ParserWithTerm(val parser: Parser, val term: Term) extends Product with Serializable
// // sealed abstract class ParserWithTerm extends Product with Serializable {
// //   def parser: Parser
// //   def term: Term
// // }
// object ParserWithTerm {
//   import Parser._

//   // TODO: this still sucks ugh
//   // final case class NonTerminal(ref: Symbol)(implicit doc: SemanticDocument) extends ParserWithTerm {
//   //   val parser = Parser.NonTerminal(ref)
//   //   val term = Term.Name(ref.info.get.displayName) // TODO: I think this is correct, but needs checking
//   // }

//   final case class NonTerminal(parser: Parser.NonTerminal, term: Term) extends ParserWithTerm(parser, term)
//   object NonTerminal {
//     def apply(ref: Symbol)(implicit doc: SemanticDocument): NonTerminal = {
//       NonTerminal(Parser.NonTerminal(ref), Term.Name(ref.info.get.displayName))
//     }
//   }

//   def apply(term: Term)(implicit env: Map[Symbol, NonTerminalTree], doc: SemanticDocument): ParserWithTerm = {
//     val parser = term match {
//       // See https://scalacenter.github.io/scalafix/docs/developers/symbol-matcher.html#unapplytree for how to mitigate
//       // against matching multiple times using SymbolMatchers

//       case t if env.contains(t.symbol) => NonTerminal(t.symbol)

//       // "string(s)"
//       case Term.Apply.After_4_6_0(Matchers.string(_), Term.ArgClause(List(s), _)) => Str(s.toString)
//       // "empty"
//       case Matchers.empty(Term.Name(_)) => Empty
//       // "pure(x)"
//       case Term.Apply.After_4_6_0(Matchers.pure(_), Term.ArgClause(List(x), _)) => Pure(x)
//       // "p.map(f)"
//       case Term.Apply.After_4_6_0(Term.Select(qual, Matchers.map(_)), Term.ArgClause(List(f), _)) => FMap(apply(qual).parser, f)
//       // "p <|> q" or "p | q"
//       case Term.ApplyInfix.After_4_6_0(p, Matchers.<|>(_), _, Term.ArgClause(List(q), _)) => Or(apply(p).parser, apply(q).parser)
//       // "p <*> q"
//       case Term.ApplyInfix.After_4_6_0(p, Matchers.<*>(_), _, Term.ArgClause(List(q), _)) => <*>(apply(p).parser, apply(q).parser)

//       case _ => Unknown
//     }

//     ParserWithTerm(parser, term)
//   }
// }
