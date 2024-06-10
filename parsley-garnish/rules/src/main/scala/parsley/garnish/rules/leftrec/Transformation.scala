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

    for (sym <- nonTerminals.keys) {
      val unfolded = unfold(nonTerminals.toMap, sym)
      val transformedParser = transform(unfolded, nonTerminals(sym).tpe)
      if (transformedParser.isDefined) {
        nonTerminals(sym) = nonTerminals(sym).copy(parser = transformedParser.get)
      }
    }

    // TODO: make patches atomic?
    nonTerminals.values.map {
      case ParserDefinition(_, transformed, _, originalTree) =>
          Patch.replaceTree(originalTree, transformed.term.syntax)
    }.asPatch
  }

  /* Returns a parser transformed into postfix form if it is left-recursive, otherwise returns None. */
  private def transform(unfolded: UnfoldedParser, tpe: Type.Name): Option[Parser] = {
    val UnfoldedParser(empty, nonLeftRec, leftRec) = unfolded
    val empties = empty match {
      case None => Empty
      case Some(t) => Pure(t)
    }

    leftRec.prettify match {
      case Empty => None
      // case Pure(_) => None  // TODO: special case: report infinite loop which couldn't be left factored -- should this be looking for any parser which can parse empty?
      // TODO: import postfix if not in scope
      // TODO: report can't left factor if there are impure parsers
      case leftRec =>
        println(s">>> ${Postfix(tpe, nonLeftRec <|> empties, leftRec)}")
        // println(s">>> POSTFIX: empties = ${empties.simplify}, nonLeftRec = ${nonLeftRec.simplify}, leftRec = ${leftRec.simplify}")
        val result = Some(Postfix(tpe, nonLeftRec <|> empties, leftRec).prettify)
        println(result)
        result
    }
  }

  private def unfold(env: Map[Symbol, ParserDefinition], nonTerminal: Symbol)(implicit doc: SemanticDocument): UnfoldedParser = {
    env(nonTerminal).parser.unfold(UnfoldingContext(Set.empty, env, nonTerminal), doc)
  }
}
