package fix

import scalafix.v1._
import scala.meta._

// TODO: tidy up
package object utils {

  def getType(sig: Signature): Option[SemanticType] = sig match {
    // which symbols have value/method sigs differs between scala versions?
    case ValueSignature(tpe)               => Some(tpe)
    case MethodSignature(_, _, returnType) => Some(returnType)
    // TODO: class types?
    case _ => None
  }

  def isParsleyType(s: Symbol)(implicit doc: SemanticDocument): Boolean = {
    s.info match {
      case Some(symInfo) => getType(symInfo.signature) match {
          // parameterised type: Parsley[_]
          case Some(TypeRef(_, matchers.parsley(_), _)) => true
          case _                                        => false
        }
      case _ => false
    }
  }

  def getNonTerminals(implicit doc: SemanticDocument): Map[Symbol, (Term.Name, Term)] = {
    doc.tree.collect {
      // see https://scalameta.org/docs/semanticdb/specification.html#symbol for symbol uniqueness guarantees
      // since we only deal with one document at a time, it should be fine to look at both global and local symbols

      // value declarations inferred to have a Parsley[_] type
      case valDefn @ Defn.Val(_, List(Pat.Var(name)), _, rhs) if isParsleyType(valDefn.symbol) => {
        // println(s"$v: $rhs | ${rhs.getClass()} | ${rhs.synthetics}")
        // recursivelyPrintSynthetics(rhs)
        valDefn.symbol -> (name, rhs)
      }

      case varDefn @ Defn.Var.After_4_7_2(_, List(Pat.Var(name)), _, body) if isParsleyType(varDefn.symbol) => {
        varDefn.symbol -> (name, body)
      }

      case defDefn @ Defn.Def.After_4_7_3(_, name, _, _, body) if isParsleyType(defDefn.symbol) => {
        // TODO: do we need to do something special to deal with the function arguments?
        defDefn.symbol -> (name, body)
      }
    }.toMap

    // TODO: WE CAN GET WHICH STRING IMPLICIT VIA SYNTHETICS
    // TODO: can we get the full qualified symbol of the e.g. stringLift function from the synthetics?
  }

  def prettyPrintNonTerminals(implicit doc: SemanticDocument): Unit = {
    // note: when the rhs is pretty-printed, it uses the display name rather than the symbol name
    getNonTerminals.foreach { case (sym, (name, body)) => println(s"$name ($sym): $body") }
  }

  def recursivelyPrintSynthetics(t: Term)(implicit doc: SemanticDocument): Unit = {
    println(s"synthetics $t: ${t.synthetics}")

    t match {
      case infixT @ Term.ApplyInfix.After_4_6_0(lhs, op, _, args) => {
        println(s"syntheticOperator $infixT: ${infixT.syntheticOperators}")
        recursivelyPrintSynthetics(lhs)
        recursivelyPrintSynthetics(op)
        args.foreach(recursivelyPrintSynthetics)
      }

      case Term.Apply.After_4_6_0(fun, args) => {
        recursivelyPrintSynthetics(fun)
        args.foreach(recursivelyPrintSynthetics)
      }

      case Term.Function.After_4_6_0(params, body) => {
        recursivelyPrintSynthetics(body)
      }

      case Term.AnonymousFunction(body) => {
        recursivelyPrintSynthetics(body)
      }

      case Term.Placeholder() => {}

      case lit: Lit => {}

      // too hard to make this exhaustive
      case Term.ArgClause(_, _) => {}
      case Term.This(_)         => {}
      case Term.Super(_, _)     => {}
      case Term.Name(_)         => {}
      case Term.Anonymous()     => {}
      case Term.Select(qual, name) => {
        recursivelyPrintSynthetics(qual)
        recursivelyPrintSynthetics(name)
      }
      case Term.Interpolate(name, _, args) => {
        recursivelyPrintSynthetics(name)
        args.foreach(recursivelyPrintSynthetics)
      }
      case Term.Xml(parts, args) => {
        parts.foreach(recursivelyPrintSynthetics)
        args.foreach(recursivelyPrintSynthetics)
      }
    }
  }
}
