package fix.utils

import scala.meta._
import scalafix.v1._

import PartialFunction.cond

object TypeUtils {
  def getSemanticType(sig: Signature): Option[SemanticType] = sig match {
    // which symbols have value/method signatures seems to differ between scala versions
    case ValueSignature(tpe)               => Some(tpe)
    case MethodSignature(_, _, returnType) => Some(returnType)
    // do ClassSignatures hold any useful info?
    case _ => None
  }

  /** Retrieves the type of which a Parsley parser would return,
    * i.e. returns the name of type T given a symbol with type Parsley[T].
   */
  def getParsleyType(s: Symbol)(implicit doc: SemanticDocument): Option[Type.Name] = {
    s.info.flatMap(info => getSemanticType(info.signature)).collect {
      case TypeRef(_, Matchers.parsley(_), List(t)) => Type.Name(t.toString)
    }
  }

  def isParsleyType(s: Symbol)(implicit doc: SemanticDocument): Boolean = cond(s.info) {
    case Some(info) => cond(getSemanticType(info.signature)) {
      // parameterised type: Parsley[_]
      case Some(TypeRef(_, Matchers.parsley(_), _)) => true
    }
  }

  case class AnnotatedType(tpe: SemanticType, isGeneric: Boolean = false)
  
  def collectInferredType(synthetics: List[SemanticTree]): List[List[SemanticType]] = {
    def extractParamListsTypes(info: SymbolInformation): List[List[AnnotatedType]] = info.signature match {
      case MethodSignature(genericTypeParams, paramLists, _) => {
        val genericTypeParamSymbols = genericTypeParams.map(_.symbol).toSet
        paramLists.map(_.collect(_.signature match {
          case ValueSignature(tpe) => tpe match {
            case TypeRef(_, sym, _) => {
              if (genericTypeParamSymbols contains sym) AnnotatedType(tpe, isGeneric = true)
              else AnnotatedType(tpe)
            }
            case _ => AnnotatedType(tpe)
          }
        }))
      }
      case _ => List.empty
    }

    val typeList = synthetics collect {
      // method had generic parameters
      case TypeApplyTree(SelectTree(_, IdTree(info)), concreteTypeArgs) => {
        val signatureShape = extractParamListsTypes(info) // e.g. List(List(A, Int), List(B)), where some of the types are generic
        substituteTypes(signatureShape, concreteTypeArgs) // substitutes the concrete types in place of the generic ones
      }
      // method had no generic parameters
      case SelectTree(_, IdTree(info)) => extractParamListsTypes(info).map(_.map(_.tpe))
    }

    assert(typeList.length <= 1, s"expected at most one inferred type signature, got $typeList")
    typeList.headOption.getOrElse(List.empty)
  }

  def substituteTypes(typeSignature: List[List[AnnotatedType]], concreteTypes: List[SemanticType]): List[List[SemanticType]] = {
    @annotation.tailrec
    def recurse(nested: List[List[AnnotatedType]], flat: List[SemanticType], acc: List[List[SemanticType]]): List[List[SemanticType]] = {

      @annotation.tailrec
      def substituteParamList(params: List[AnnotatedType], concreteTypes: List[SemanticType], acc: List[SemanticType]): (List[SemanticType], List[SemanticType]) =
        params match {
          case Nil => (acc.reverse, concreteTypes)
          case head :: next => {
            if (head.isGeneric) substituteParamList(next, concreteTypes.tail, concreteTypes.head :: acc)
            else substituteParamList(next, concreteTypes, head.tpe :: acc)
          }
      }

      nested match {
        case Nil => acc.reverse  // reversing at the end because we are building the list backwards
        case paramList :: next => {
          val (replacedParamList, remainingConcreteTypes) = substituteParamList(paramList, flat, List.empty)
          recurse(next, remainingConcreteTypes, replacedParamList :: acc)
        }
      }
    }

    if (typeSignature.flatten.count(_.isGeneric) != concreteTypes.length) List.empty
    else recurse(typeSignature, concreteTypes, List.empty)
  }
}