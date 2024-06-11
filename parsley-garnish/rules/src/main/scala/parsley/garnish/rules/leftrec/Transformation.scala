package parsley.garnish.rules.leftrec

import scala.collection.mutable
import scala.meta._
import scalafix.v1._

import parsley.garnish.analysis.ParserTransformer.{getNonTerminalParserDefns, ParserDefinition}
import parsley.garnish.model.Parser, Parser._

object Transformation {
  def removeLeftRecursion()(implicit doc: SemanticDocument): Patch = {
    val nonTerminals = getNonTerminalParserDefns.map { parserDefn =>
      parserDefn.name.symbol -> parserDefn
    }.to(mutable.Map)

    // Rewrite transformed parsers back into the map of non-terminals, if they have been transformed
    // Also collect lints emitted during the transformation process
    val lints = nonTerminals.keysIterator.toSeq.map { sym =>
      val unfolded = unfold(nonTerminals.toMap, sym)
      transform(unfolded, nonTerminals(sym)) match {
        case Left(patch) => patch
        case Right(transformedParser) =>
          nonTerminals(sym) = nonTerminals(sym).copy(parser = transformedParser)
          Patch.empty
      }
    }

    // TODO: make patches atomic?
    lints.asPatch + nonTerminals.values.map {
      case ParserDefinition(_, transformed, _, originalTree) =>
          Patch.replaceTree(originalTree, transformed.term.syntax)
    }.asPatch
  }

  /* Returns a parser transformed into postfix form if it is left-recursive, otherwise returns None. */
  private def transform(unfolded: UnfoldedParser, parserDefn: ParserDefinition): Either[Patch, Parser] = {
    val UnfoldedParser(empty, nonLeftRec, leftRec) = unfolded
    val empties = empty match {
      case None => Empty
      case Some(t) => Pure(t)
    }

    println(s">>>${parserDefn.name.syntax}<<< EMPTY = ${empties.prettify} | NONLEFTREC = ${nonLeftRec} | LEFTREC = ${leftRec.prettify}")

    leftRec.normalise match {
      case Empty => Left(Patch.empty)
      case _: Pure => Left(Patch.lint(
        LeftRecDerivesEmptyLint(parserDefn, Postfix(parserDefn.tpe, nonLeftRec <|> empties, leftRec).prettify)
      ))
      // TODO: import postfix if not in scope
      // TODO: report can't left factor if there are impure parsers
      case leftRec =>
        // println(s">>> ${Postfix(tpe, nonLeftRec <|> empties, leftRec)}")
        // println(s">>> POSTFIX: empties = ${empties.simplify}, nonLeftRec = ${nonLeftRec.simplify}, leftRec = ${leftRec.simplify}")
        Right(Postfix(parserDefn.tpe, nonLeftRec <|> empties, leftRec).prettify)
    }
  }

  private def unfold(env: Map[Symbol, ParserDefinition], nonTerminal: Symbol)(implicit doc: SemanticDocument): UnfoldedParser = {
    env(nonTerminal).parser.unfold(UnfoldingContext(Set.empty, env, nonTerminal), doc)
  }
}
