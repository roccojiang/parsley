package parsley.garnish.parser

import scala.meta._
import scalafix.v1._

import parsley.garnish.implicits.TermOps
import parsley.garnish.utils.TypeUtils.{getParsleyType, isParsleyType}

object GrammarExtractor {
  type Grammar = Map[Symbol, ParserDefinition]

  final case class ParserDefinition(name: Term.Name, parser: Parser, tpe: Type.Name, originalTree: Term) {
    val definitionSite: Position = originalTree.parent.getOrElse(originalTree).pos
  }

  def getParserDefinitions(includeDefDefinitions: Boolean = true)
                          (implicit doc: SemanticDocument): Seq[ParserDefinition] = {
    val varDefns = doc.tree.collect {
      // See https://scalameta.org/docs/semanticdb/specification.html#symbol for symbol uniqueness guarantees
      // since we only deal with one document at a time, it should be fine to look at both global and local symbols
      case VariableDecl(vars, body) => collectVars(vars, body)
    }.flatten

    val defDefns = doc.tree.collect {
      case defDefn @ Defn.Def.After_4_7_3(_, name, _, _, body) if isParsleyType(defDefn.symbol) =>
        // TODO: do we need to do something special to deal with the function arguments?
        Seq(buildParserDefinition(defDefn.symbol, name, body))
    }.flatten

    varDefns ++ (if (includeDefDefinitions) defDefns else Seq.empty)
  }

  def getGrammarMap(includeDefDefinitions: Boolean = false)(implicit doc: SemanticDocument): Grammar =
    getParserDefinitions(includeDefDefinitions).map(parserDefn => parserDefn.name.symbol -> parserDefn).toMap

  private object VariableDecl {
    def unapply(tree: Tree): Option[(List[Pat], Term)] = tree match {
      case Defn.Val(_, vars, _, body)             => Some((vars, body))
      case Defn.Var.After_4_7_2(_, vars, _, body) => Some((vars, body))

      case _ => None
    }
  }

  private def collectVars(vars: List[Pat], body: Term)(implicit doc: SemanticDocument): Seq[ParserDefinition] =
    vars.collect {
      case Pat.Var(varName) if isParsleyType(varName.symbol) =>
        buildParserDefinition(varName.symbol, varName, body)
    }

  private def buildParserDefinition(sym: Symbol, name: Term.Name, body: Term)
                                   (implicit doc: SemanticDocument): ParserDefinition = {
    val tpe = getParsleyType(sym)
    assert(tpe.isDefined, s"expected a Parsley type for $name, got ${sym.info.get.signature}")
    ParserDefinition(name, body.toParser, tpe.get, body)
  }
}
