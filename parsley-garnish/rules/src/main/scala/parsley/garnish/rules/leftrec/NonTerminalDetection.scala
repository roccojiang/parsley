package parsley.garnish.rules.leftrec

import scala.meta._
import scalafix.v1._

import parsley.garnish.model.Parser
import parsley.garnish.utils.{getParsleyType, isParsleyType}

object NonTerminalDetection {

  final case class ParserDefinition(name: Term.Name, parser: Parser, tpe: Type.Name, originalTree: Term)

  def getNonTerminals(implicit doc: SemanticDocument): Seq[ParserDefinition] =
    doc.tree.collect {
      // see https://scalameta.org/docs/semanticdb/specification.html#symbol for symbol uniqueness guarantees
      // since we only deal with one document at a time, it should be fine to look at both global and local symbols

      // value declarations inferred to have a Parsley[_] type
      case Defn.Val(_, vars, _, body) => collectVars(vars, body)
      case Defn.Var.After_4_7_2(_, vars, _, body) => collectVars(vars, body)
      // this case actually probably shouldn't count as a non-terminal
      /*
      case defDefn @ Defn.Def.After_4_7_3(_, name, _, _, body) if isParsleyType(defDefn.symbol) => {
        // TODO: do we need to do something special to deal with the function arguments?
        List(defDefn.symbol -> NonTerminalTree(name, body, defDefn))
      }
      */
    }.flatten

  private def collectVars(vars: List[Pat], body: Term)(implicit doc: SemanticDocument): Seq[ParserDefinition] =
    vars.collect {
      case Pat.Var(varName) if isParsleyType(varName.symbol) =>
        val tpe = getParsleyType(varName.symbol)
        assert(tpe.isDefined, s"expected a Parsley type for $varName, got ${varName.symbol.info.get.signature}")

        ParserDefinition(varName, Parser(body), tpe.get, body)
    }

    // TODO: WE CAN GET WHICH STRING IMPLICIT VIA SYNTHETICS
    // TODO: can we get the full qualified symbol of the e.g. stringLift function from the synthetics?
}
