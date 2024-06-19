package parsley.garnish.rules.leftrec

import scala.collection.mutable
import scala.meta._
import scalafix.v1._

import parsley.garnish.analysis.ParserTransformer.{getNonTerminalParserDefns, ParserDefinition}
import parsley.garnish.model.Parser, Parser._

object Transformation {
  def removeLeftRecursion()(implicit doc: SemanticDocument): Patch = {
    val nonTerminals = getNonTerminalParserDefns.map(_.name.symbol)
    val grammarMap = getNonTerminalParserDefns.map { parserDefn =>
      parserDefn.name.symbol -> (parserDefn.parser, parserDefn)
    }.to(mutable.Map)

    // println(nonTerminals)

    // Rewrite transformed parsers back into the map of non-terminals, if they have been transformed
    // Also collect lints emitted during the transformation process
    val lints = nonTerminals.map { sym =>
      val unfolded = unfold(grammarMap.view.mapValues(_._2).toMap, sym)
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

  private def unfold(env: Grammar, nonTerminal: Symbol)(implicit doc: SemanticDocument): UnfoldedParser =
    env(nonTerminal).parser.unfold(UnfoldingContext(Set.empty, env, nonTerminal), doc)
}
