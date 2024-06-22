package parsley.garnish.rules.leftrec

import scala.collection.mutable
import scala.meta._
import scalafix.v1._

import parsley.garnish.parser.GrammarExtractor._
import parsley.garnish.rules.leftrec.Transformation._

class FactorLeftRecursion extends SemanticRule("FactorLeftRecursion") {
  override def fix(implicit doc: SemanticDocument): Patch = {
    val nonTerminals = getParserDefinitions(includeDefDefinitions = false).map(_.name.symbol)
    val grammarMap = getGrammarMap().map { 
      case (sym, parserDefn) => sym -> (parserDefn.parser, parserDefn)
    }.to(mutable.Map)

    // Rewrite transformed parsers back into the map of non-terminals, if they have been transformed
    // Also collect lints emitted during the transformation process
    val lints = nonTerminals.map { sym =>
      val unfolded = unfoldProduction(grammarMap.view.mapValues(_._2).toMap, sym)
      val (orig, parserDefn) = grammarMap(sym)
      transform(unfolded, parserDefn) match {
        case Left(patch) => patch.atomic
        case Right(transformedParser) =>
          grammarMap(sym) = (orig, parserDefn.copy(parser = transformedParser))
          Patch.empty
      }
    }.asPatch

    val rewrites = grammarMap.values.collect {
      case (original, ParserDefinition(_, transformed, _, originalTree)) if !original.isEquivalent(transformed) =>
        Patch.replaceTree(originalTree, transformed.term.syntax)
    }.asPatch + Patch.addGlobalImport(importer"parsley.expr.chain")

    // TODO: more principled manner of determining which imports to add

    // Lints can be individually atomic patches, as they are safe to report in isolation
    // Rewrites are wrapped in a single atomic patch: left-recursion removal depends on *all* productions being factored
    lints + rewrites.atomic
  }
}
