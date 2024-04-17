package fix.utils

import scala.meta._
import scalafix.v1._

object NonTerminalDetection {

  case class NonTerminalTree(name: Term.Name, body: Term, originalTree: Defn)

  def getNonTerminals(implicit doc: SemanticDocument): Map[Symbol, NonTerminalTree] = {
    def collectVars(vars: List[Pat], body: Term, originalTree: Defn) = {
      vars.collect {
        case Pat.Var(varName) if isParsleyType(varName.symbol) =>
          varName.symbol -> NonTerminalTree(varName, body, originalTree)
      }
    }

    // doc.tree.children

    doc.tree.collect {
      // see https://scalameta.org/docs/semanticdb/specification.html#symbol for symbol uniqueness guarantees
      // since we only deal with one document at a time, it should be fine to look at both global and local symbols

      // value declarations inferred to have a Parsley[_] type
      case valDefn @ Defn.Val(_, vars, _, body) => collectVars(vars, body, valDefn)
      case varDefn @ Defn.Var.After_4_7_2(_, vars, _, body) => collectVars(vars, body, varDefn)
      case defDefn @ Defn.Def.After_4_7_3(_, name, _, _, body) if isParsleyType(defDefn.symbol) => {
        // TODO: do we need to do something special to deal with the function arguments?
        List(defDefn.symbol -> NonTerminalTree(name, body, defDefn))
      }
    }.flatten.toMap

    // TODO: WE CAN GET WHICH STRING IMPLICIT VIA SYNTHETICS
    // TODO: can we get the full qualified symbol of the e.g. stringLift function from the synthetics?
  }
}
