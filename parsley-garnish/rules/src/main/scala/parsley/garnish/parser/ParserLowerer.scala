package parsley.garnish.parser

import scala.meta._

import Parser._

private[parser] object ParserLowerer {
  def lower(parser: Parser): Term = parser match {
    case p: CoreParser => lowerCore(p)
    case p: LiftParser => lowerLift(p)
    case p: CharacterParser => lowerChar(p)
    case p: SequenceParser => lowerSeq(p)
    case p: ChainParser => lowerChain(p)
    case p: IterativeParser => lowerIter(p)
    case p: SeparatedValuesParser => lowerSepVal(p)

    case Unknown(term) => term
  }

  private def lowerCore(p: CoreParser) = p match {
    case NonTerminal(_, name) => Term.Name(name)
    case Pure(x) => q"pure(${x.term})"
    case Empty => Term.Name("empty")
    case p <|> q => q"${p.term} | ${q.term}" // TODO: preserve original combinator choice (i.e. <|> or |)
    case p <*> q => q"${p.term} <*> ${q.term}"
  }

  private def lowerLift(p: LiftParser) = p match {
    case FMap(p, f) => q"${p.term}.map(${f.term})"
    case LiftExplicit(f, ps) =>
      val liftN = Term.Name(s"lift${ps.length}")
      q"$liftN(${f.term}, ..${ps.map(_.term)})"
    case LiftImplicit(f, ps) => q"${f.term}.lift(..${ps.map(_.term)})"
    case Zipped(f, ps) => q"(..${ps.map(_.term)}).zipped(${f.term})"
    case Bridge(f, ps) => q"${f.term}(..${ps.map(_.term)})"
  }

  private def lowerChar(p: CharacterParser) = p match {
    case Str(s, implicitSyntax) => if (implicitSyntax) Lit.String(s) else q"string($s)"
    case Chr(c, implicitSyntax) => if (implicitSyntax) Lit.Char(c) else q"char($c)"
  }

  private def lowerSeq(p: SequenceParser) = p match {
    case p ~> q => q"${p.term} ~> ${q.term}"
    case p <~ q => q"${p.term} <~ ${q.term}"
  }

  private def lowerChain(p: ChainParser) = p match {
    case Postfix(tpe, p, op) => q"chain.postfix[$tpe](${p.term})(${op.term})"
  }

  private def lowerIter(p: IterativeParser) = p match {
    case ManyP(p) => q"many(${p.term})"
    case SomeP(p) => q"some(${p.term})"
  }

  private def lowerSepVal(p: SeparatedValuesParser) = p match {
    case EndBy(p, sep) => q"endBy(${p.term}, ${sep.term})"
  }
}
