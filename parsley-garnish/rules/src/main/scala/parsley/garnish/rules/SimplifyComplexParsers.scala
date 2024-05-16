package parsley.garnish.rules

import scala.meta._
import scalafix.v1._

import parsley.garnish.analysis.ParserAnalyzer.{getAllParserDefns, ParserDefinition}
import parsley.garnish.utils._
import parsley.garnish.utils.TypeUtils.getSemanticType

class SimplifyComplexParsers extends SemanticRule("SimplifyComplexParsers") {
  override def fix(implicit doc: SemanticDocument): Patch = {
    // recursivelyPrintSymbol(doc.tree)
    // val parserDefns = getAllParserDefns

    // doc.tree.traverse {
    //   case f@Term.Function.After_4_6_0(params, body) =>
    //     val syms = params.values.collect { case Term.Param(_, name: Term.Name, _, _) => name.symbol }
    //     println(s"> $f has params $syms")
    // }

    show

    doc.tree.traverse {
      case t@Term.Apply.After_4_6_0(term, _) => 
        val synthetics = term.synthetics.structure
        val tpe = term.symbol.info.map(i => getSemanticType(i.signature))

        term.synthetics.foreach {
          case SelectTree(qualifier, IdTree(symInfo)) => 
            val isMatch = Matchers.bridgeApply.matches(symInfo.symbol)
            println(s"SYMBOLMATCH >> $t with $symInfo: $isMatch")
          case _ =>
        }
        println(s">> $t: $synthetics")
    }

    getAllParserDefns.map { case ParserDefinition(_, parser, _, originalTree) =>
      println(parser)
      val simplifiedParser = parser.simplify
      if (simplifiedParser != parser) {
        val simplifiedParserTerm = simplifiedParser.term.syntax
        Patch.replaceTree(originalTree, simplifiedParserTerm)
      } else {
        Patch.empty
      }
    }.asPatch
  }
}
