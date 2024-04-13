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

      case Term.Function.After_4_6_0(_, body) => {
        recursivelyPrintSynthetics(body)
      }

      case Term.AnonymousFunction(body) => {
        recursivelyPrintSynthetics(body)
      }

      case Term.Placeholder() => {}

      case _: Lit => {}

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

      case _ => {}
    }
  }
}
