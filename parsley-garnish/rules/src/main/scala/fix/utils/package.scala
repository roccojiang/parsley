package fix

import scalafix.v1._
import scala.meta._

import PartialFunction.cond

package object utils {

  def getSemanticType(sig: Signature): Option[SemanticType] = sig match {
    // which symbols have value/method signatures seems to differ between scala versions
    case ValueSignature(tpe)               => Some(tpe)
    case MethodSignature(_, _, returnType) => Some(returnType)
    // do ClassSignatures hold any useful info?
    case _ => None
  }

  def getSymbolType(s: Symbol)(implicit doc: SemanticDocument): Option[Type.Name] = {
    s.info.flatMap(info => getSemanticType(info.signature)).collect {
      case TypeRef(_, _, List(t)) => Type.Name(t.toString)
    }
  }

  def isParsleyType(s: Symbol)(implicit doc: SemanticDocument): Boolean = cond(s.info) {
    case Some(info) => cond(getSemanticType(info.signature)) {
      // parameterised type: Parsley[_]
      case Some(TypeRef(_, Matchers.parsley(_), _)) => true
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
