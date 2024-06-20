package parsley.garnish.rules.leftrec

import scala.collection.mutable
import scala.meta._
import scalafix.v1._

import parsley.garnish.parser.ParserLifter._
import parsley.garnish.expr.Expr, Expr._
import parsley.garnish.parser.Parser
import Parser._

object Transformation {
  def removeLeftRecursion()(implicit doc: SemanticDocument): Patch = {
    val nonTerminals = getParserDefinitions().map(_.name.symbol)
    val grammarMap = getGrammarMap().map { 
      case (sym, parserDefn) => sym -> (parserDefn.parser, parserDefn)
    }.to(mutable.Map)

    // Rewrite transformed parsers back into the map of non-terminals, if they have been transformed
    // Also collect lints emitted during the transformation process
    val lints = nonTerminals.map { sym =>
      val unfolded = unfoldProduction(grammarMap.view.mapValues(_._2).toMap, sym)
      val (orig, parserDefn) = grammarMap(sym)
      transform(unfolded, parserDefn) match {
        case Left(patch) => patch
        case Right(transformedParser) =>
          grammarMap(sym) = (orig, parserDefn.copy(parser = transformedParser))
          Patch.empty
      }
    }.asPatch

    val rewrites = grammarMap.values.collect {
      case (original, ParserDefinition(_, transformed, _, originalTree)) if !original.isEquivalent(transformed) =>
        Patch.replaceTree(originalTree, transformed.term.syntax)
    }.asPatch

    // TODO: make patches atomic?
    // TODO: more principled manner of determining which imports to add
    lints + rewrites + Patch.addGlobalImport(importer"parsley.expr.chain")
  }

  /* Returns a parser transformed into postfix form if it is left-recursive. */
  private def transform(unfolded: UnfoldedParser, parserDefn: ParserDefinition): Either[Patch, Parser] = {
    val UnfoldedParser(results, nonLeftRec, leftRec) = unfolded
    val result = results match {
      case Some(t) => Pure(t)
      case None    => Empty
    }

    val transformed = Postfix(parserDefn.tpe, nonLeftRec | result, leftRec).prettify

    leftRec.normalise match {
      // Not left-recursive, do not rewrite
      case Empty   => Left(Patch.empty)
      // Left-recursive but unfixable, emit error lint
      case Pure(_) => Left(Patch.lint(LeftRecDerivesEmptyLint(parserDefn, transformed)))
      // Left-recursive and fixable, rewrite
      case _       => Right(transformed)
    }
  }

  private case class UnfoldedParser(results: Option[Expr], nonLeftRec: Parser, leftRec: Parser) {
    val isLeftRecursive = leftRec.normalise != Parser.Empty
  }
  private case class UnfoldingContext(visited: Set[Symbol], env: Grammar, nonTerminal: Symbol)

  private def unfoldProduction(env: Grammar, nonTerminal: Symbol)(implicit doc: SemanticDocument): UnfoldedParser = {
    implicit val emptyCtx = UnfoldingContext(Set.empty, env, nonTerminal)
    unfold(env(nonTerminal).parser)
  }

  private def unfold(parser: Parser)(implicit ctx: UnfoldingContext, doc: SemanticDocument): UnfoldedParser = parser match {
    case p: CoreParser => unfoldCore(p)
    case p: LiftParser => unfoldLift(p)
    case p: CharacterParser => UnfoldedParser(None, p, Empty)
    case p: SequenceParser => unfoldSeq(p)
    case p: ChainParser => UnfoldedParser(None, p, Empty) // TODO: unfold postfix properly
    case p: IterativeParser => unfoldIter(p)
    case p: SeparatedValuesParser => unfoldSepVal(p)

    case p: Unknown => UnfoldedParser(None, p, Empty)
  }

  private def unfoldCore(p: CoreParser)(implicit ctx: UnfoldingContext, doc: SemanticDocument) = p match {
    case p: NonTerminal => unfoldNT(p)
    case Pure(x) => UnfoldedParser(Some(x), Empty, Empty)
    case Empty => UnfoldedParser(None, Empty, Empty)
    case p <|> q => unfoldChoice(p, q)
    case p <*> q => unfoldAp(p, q)
  }

  private def unfoldNT(nt: NonTerminal)(implicit ctx: UnfoldingContext, doc: SemanticDocument) = {
    assert(ctx.env.contains(nt.ref),
      s"expected to find non-terminal ${nt.ref} in this file, instead found: ${ctx.env.keys}")

    if (nt.ref == ctx.nonTerminal) {
      UnfoldedParser(None, Empty, Pure(id))
    } else if (ctx.visited.contains(nt.ref)) {
      UnfoldedParser(None, nt, Empty)
    } else {
      val unfoldedRef = unfold(ctx.env(nt.ref).parser)(ctx.copy(visited = ctx.visited + nt.ref), doc)

      if (unfoldedRef.results.isEmpty && !unfoldedRef.isLeftRecursive) {
        // The non-terminal we recursively unfolded was not left-recursive, so we just reference its name directly,
        // rather than aggressively inlining it
        UnfoldedParser(None, nt, Empty)
      } else {
        // In the left-recursive case, or if it has a semantic action,
        // we must inline the result of the unfolded non-terminal
        unfoldedRef
      }
    }
  }

  private def unfoldChoice(p: Parser, q: Parser)(implicit ctx: UnfoldingContext, doc: SemanticDocument) = {
    val UnfoldedParser(pe, pn, pl) = unfold(p)
    val UnfoldedParser(qe, qn, ql) = unfold(q)

    UnfoldedParser(pe.orElse(qe), pn | qn, pl | ql)
  }

  private def unfoldAp(p: Parser, q: Parser)(implicit ctx: UnfoldingContext, doc: SemanticDocument) = {
    val UnfoldedParser(pe, pn, pl) = unfold(p)
    val UnfoldedParser(qe, qn, ql) = unfold(q)

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

  private def unfoldLift(p: LiftParser)(implicit ctx: UnfoldingContext, doc: SemanticDocument) = p match {
    case FMap(p, f) => unfold(Pure(f) <*> p)
    case _ => 
      val curriedFunc: Parser = Pure(p.func match {
        // The dodgy case: had to treat the entire function as opaque
        case Translucent_(f, substs) if p.parsers.size > 1 => Translucent(f, substs, isCurried = true)
        // The normal case: this function should've been lifted to Expr correctly, so currying actually works normally
        case _ => p.func.curried
      })

      unfold(p.parsers.foldLeft(curriedFunc)(_ <*> _))
  }

  private def unfoldSeq(p: SequenceParser)(implicit ctx: UnfoldingContext, doc: SemanticDocument) = p match {
    case p ~> q =>
      val (x, y) = (Var.fresh(), Var.fresh())
      val f = Abs(x, Abs(y, y)) // const id = \x y -> y
      unfold(p.map(f) <*> q)

    case p <~ q =>
      val (x, y) = (Var.fresh(), Var.fresh())
      val f = Abs(x, Abs(y, x)) // const = \x y -> x
      unfold(p.map(f) <*> q)
  }

  private def unfoldIter(p: IterativeParser)(implicit ctx: UnfoldingContext, doc: SemanticDocument) = p match {
    case ManyP(p) =>
      val UnfoldedParser(_, pn, pl) = unfold(p)

      val lefts = pl.map {
        val (f, xs, nt) = (Var.fresh(), Var.fresh(), Var.fresh())
        Abs(f, Abs(xs, Abs(nt, cons(App(f, nt), xs)))) // \f xs nt -> f nt : xs
      } <*> ManyP(p)

      val nonLefts = SomeP(pn)

      UnfoldedParser(Some(Translucent(q"Nil")), nonLefts, lefts)

    case SomeP(p) => unfold(p.map(cons) <*> ManyP(p))
  }

  private def unfoldSepVal(p: SeparatedValuesParser)(implicit ctx: UnfoldingContext, doc: SemanticDocument) = p match {
    case EndBy(p, sep) => unfold(ManyP(p <~ sep))
  }
}
