package fix

import scalafix.v1._
import scala.meta._

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

  def getNonTerminals(implicit doc: SemanticDocument): Map[Symbol, Term] = {
    doc.tree.collect {
      // value declarations inferred to have a Parsley[_] type
      case v @ Defn.Val(_, _, _, rhs) => {
      // case v @ Defn.Val(_, _, _, rhs) if isParsleyType(v.symbol) => {
        // println(s"$v: $rhs | ${rhs.getClass()} | ${rhs.synthetics}")
        recursivelyPrintSynthetics(rhs)
        v.symbol -> rhs
      }
    }.toMap

    // WE CAN GET WHICH STRING IMPLICIT VIA SYNTHETICS
    // TODO: can we get the full qualified symbol of the e.g. stringLift function from the synthetics?
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
