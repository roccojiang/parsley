package parsley.garnish.analysis

import scala.meta._
import scalafix.v1._

import parsley.garnish.model.Parser
import parsley.garnish.utils.TypeUtils.{getParsleyType, isParsleyType}
import parsley.garnish.implicits.TermOps

object ParserAnalyzer {

  final case class ParserDefinition(name: Term.Name, parser: Parser, tpe: Type.Name, originalTree: Term)

  def getNonTerminalParserDefns(implicit doc: SemanticDocument): Seq[ParserDefinition] =
    doc.tree.collect {
      // See https://scalameta.org/docs/semanticdb/specification.html#symbol for symbol uniqueness guarantees
      // since we only deal with one document at a time, it should be fine to look at both global and local symbols
      case VariableDecl(vars, body) => collectVars(vars, body)
    }.flatten

  def getAllParserDefns(implicit doc: SemanticDocument): Seq[ParserDefinition] =
    doc.tree.collect {
      case VariableDecl(vars, body) => collectVars(vars, body)
      // this case lives here at the moment, I don't think it counts as a non-terminal
      case defDefn @ Defn.Def.After_4_7_3(_, name, _, _, body) if isParsleyType(defDefn.symbol) =>
        // TODO: do we need to do something special to deal with the function arguments?
        Seq(buildParserDefinition(defDefn.symbol, name, body))
    }.flatten

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

  // TODO: WE CAN GET WHICH STRING IMPLICIT VIA SYNTHETICS
  // TODO: can we get the full qualified symbol of the e.g. stringLift function from the synthetics?
}
