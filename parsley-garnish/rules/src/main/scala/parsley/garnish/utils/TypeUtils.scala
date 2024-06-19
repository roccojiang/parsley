package parsley.garnish.utils

import scala.PartialFunction.cond
import scala.meta.Type
import scalafix.v1._

object TypeUtils {
  /** Retrieves the type of which a Parsley parser would return,
   * i.e. returns the name of type T given a symbol with type Parsley[T].
   */
  def getParsleyType(s: Symbol)(implicit doc: SemanticDocument): Option[Type.Name] = {
    s.info.flatMap(info => getSemanticType(info.signature)).collect {
      case TypeRef(_, parsleyMatcher(_), List(t)) => Type.Name(t.toString)
    }
  }

  def isParsleyType(s: Symbol)(implicit doc: SemanticDocument): Boolean = cond(s.info) {
    case Some(info) => cond(getSemanticType(info.signature)) {
      // Parameterised type: Parsley[_]
      case Some(TypeRef(_, parsleyMatcher(_), _)) => true
    }
  }

  def getSemanticType(sig: Signature): Option[SemanticType] = sig match {
    // Which symbols have value/method signatures seems to differ between Scala versions
    case ValueSignature(tpe)               => Some(tpe)
    case MethodSignature(_, _, returnType) => Some(returnType)
    // I don't think ClassSignatures hold any useful info?
    case _ => None
  }

  private val parsleyMatcher = SymbolMatcher.normalized("parsley.Parsley", "parsley.quick.Parsley")
}
