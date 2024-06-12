package parsley.garnish.rules.leftrec

import scala.collection.mutable
import scala.meta._
import scalafix.v1._

import parsley.garnish.analysis.ParserTransformer.{getNonTerminalParserDefns, ParserDefinition}
import parsley.garnish.model.Parser, Parser._

object Transformation {
  def removeLeftRecursion()(implicit doc: SemanticDocument): Patch = {
    val nonTerminals = getNonTerminalParserDefns.map { parserDefn =>
      parserDefn.name.symbol -> (parserDefn.parser.normaliseExprs, parserDefn)
    }.to(mutable.Map)

    // Rewrite transformed parsers back into the map of non-terminals, if they have been transformed
    // Also collect lints emitted during the transformation process
    val lints = nonTerminals.keysIterator.toSeq.map { sym =>
      val unfolded = unfold(nonTerminals.view.mapValues(_._2).toMap, sym)
      val (orig, parserDefn) = nonTerminals(sym)
      transform(unfolded, parserDefn) match {
        case Left(patch) => patch
        case Right(transformedParser) =>
          nonTerminals(sym) = (orig, parserDefn.copy(parser = transformedParser))
          Patch.empty
      }
    }.asPatch

    val rewrites = nonTerminals.values.collect {
      case (original, ParserDefinition(_, transformed, _, originalTree)) if !original.isEquivalent(transformed) =>
        Patch.replaceTree(originalTree, transformed.term.syntax)
    }.asPatch

    // TODO: make patches atomic?
    lints + rewrites
  }

  /* Returns a parser transformed into postfix form if it is left-recursive. */
  private def transform(unfolded: UnfoldedParser, parserDefn: ParserDefinition): Either[Patch, Parser] = {
    val UnfoldedParser(empty, nonLeftRec, leftRec) = unfolded
    val empties = empty match {
      case None => Empty
      case Some(t) => Pure(t)
    }

    // println(s">>>${parserDefn.name.syntax}<<< EMPTY = ${empties.prettify} | NONLEFTREC = ${nonLeftRec} | LEFTREC = ${leftRec.prettify}")

    leftRec.normalise match {
      case Empty => Left(Patch.empty)
      case _: Pure => Left(Patch.lint(
        LeftRecDerivesEmptyLint(parserDefn, Postfix(parserDefn.tpe, nonLeftRec <|> empties, leftRec).prettify)
      ))
      // TODO: import postfix if not in scope
      // https://www.javadoc.io/doc/ch.epfl.scala/scalafix-core_2.12/0.12.1/scalafix/patch/Patch$.html
      // addGlobalImport
      // perhaps add an importer for each parser, do a traversal at the end to collect all required imports
      // TODO: report can't left factor if there are impure parsers
      case leftRec =>
        val postfixed = Postfix(parserDefn.tpe, nonLeftRec <|> empties, leftRec).prettify
        println(s">>>${parserDefn.name.syntax}<<< = ${postfixed}")  
        Right(postfixed)
    }
  }

  private def unfold(env: Map[Symbol, ParserDefinition], nonTerminal: Symbol)(implicit doc: SemanticDocument): UnfoldedParser = {
    env(nonTerminal).parser.unfold(UnfoldingContext(Set.empty, env, nonTerminal), doc)
  }
}
