package fix.utils

import scala.meta._
import scalafix.v1._

import Func._

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

  // In Scala 3, this could be an opaque type
  sealed trait MethodParamType
  case class GenericType(tpe: SemanticType) extends MethodParamType
  case class ConcreteType(tpe: SemanticType) extends MethodParamType
  
  def extractParamListsTypes(info: SymbolInformation): List[List[MethodParamType]] = info.signature match {
    case MethodSignature(genericTypeParams, paramLists, _) => {
      val genericTypeParamSymbols = genericTypeParams.map(_.symbol).toSet
      paramLists.map(_.collect(_.signature match {
        case ValueSignature(tpe) => tpe match {
          case TypeRef(_, sym, _) => {
            if (genericTypeParamSymbols contains sym) GenericType(tpe)
            else ConcreteType(tpe)
          }
          case _ => ConcreteType(tpe)
        }
      }))
    }
    case _ => List.empty
  }

  def fromTypeSignature(f: UserDefined, signature: List[List[ConcreteType]]): Func = {
    val freshVars = signature.map(_.map(concreteTpe => Var(concreteTpe.tpe)))

    freshVars.foldRight(
      freshVars.foldLeft(f: Func) { (acc, params) => App(acc, params: _*) }
    ) { (params, acc) => Lam(params, acc) }
  }

  def collectInferredType(term: Term.Name)(implicit doc: SemanticDocument): List[List[ConcreteType]] = {
    val synthetics = term.synthetics
    val typeList = synthetics collect {
      // TODO: cases other than SelectTree - need to make this work for more than just .apply methods
      // method had generic parameters
      case TypeApplyTree(SelectTree(_, IdTree(info)), typeArgs) => {
        val concreteTypeArgs = typeArgs.map(ConcreteType(_))
        val signatureShape = extractParamListsTypes(info) // e.g. List(List(A, Int), List(B)), where some of the types are generic
        substituteTypes(signatureShape, concreteTypeArgs) // substitutes the concrete types in place of the generic ones
      }
      // method had no generic parameters
      case SelectTree(_, IdTree(info)) => extractParamListsTypes(info).asInstanceOf[List[List[ConcreteType]]]

      case TypeApplyTree(_: OriginalTree, typeArgs) => {
        val concreteTypeArgs = typeArgs.map(ConcreteType(_))
        val signatureShape = term.symbol.info.map(extractParamListsTypes(_))
        substituteTypes(signatureShape.get, concreteTypeArgs) // TODO: handle optional properly
      }
    }

    assert(typeList.length <= 1, s"expected at most one inferred type signature, got $typeList")
    typeList.headOption.getOrElse(List.empty)
  }

  def substituteTypes(typeSignature: List[List[MethodParamType]], concreteTypes: List[ConcreteType]): List[List[ConcreteType]] = {
    @annotation.tailrec
    def recurse(nested: List[List[MethodParamType]], flat: List[ConcreteType], acc: List[List[ConcreteType]]): List[List[ConcreteType]] = {

      @annotation.tailrec
      def substituteParamList(params: List[MethodParamType], concreteTypes: List[ConcreteType], acc: List[ConcreteType]): (List[ConcreteType], List[ConcreteType]) =
        params match {
          case Nil => (acc.reverse, concreteTypes)
          case head :: next => head match {
            case _: GenericType     => substituteParamList(next, concreteTypes.tail, concreteTypes.head :: acc)
            case head: ConcreteType => substituteParamList(next, concreteTypes, head :: acc)
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

    if (typeSignature.flatten.count(_.isInstanceOf[GenericType]) != concreteTypes.length) List.empty
    else recurse(typeSignature, concreteTypes, List.empty)
  }
}