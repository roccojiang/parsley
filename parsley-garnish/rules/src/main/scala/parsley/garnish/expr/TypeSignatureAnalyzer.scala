package parsley.garnish.expr

import scala.meta._
import scalafix.v1._

object TypeSignatureAnalyzer {

  type TypeSignature = List[List[ConcreteType]]

  // In Scala 3, this could be an opaque type
  sealed abstract class MethodParamType extends Product with Serializable {
    def isGeneric: Boolean
  }
  case class GenericType(tpe: SemanticType) extends MethodParamType {
    val isGeneric = true
  }
  case class ConcreteType(tpe: SemanticType) extends MethodParamType {
    val isGeneric = false
  }
  
  def extractParamListsTypes(info: SymbolInformation): List[List[MethodParamType]] = info.signature match {
    case MethodSignature(genericTypeParams, paramLists, _) =>
      val genericTypeParamSymbols = genericTypeParams.map(_.symbol).toSet
      paramLists.map(_.collect(_.signature match {
        case ValueSignature(tpe) => tpe match {
          case TypeRef(_, sym, _) =>
            if (genericTypeParamSymbols contains sym) GenericType(tpe)
            else ConcreteType(tpe)
          case _ => ConcreteType(tpe)
        }
      }))
    case _ => List.empty
  }

  def getInferredTypeSignature(term: Term.Name)(implicit doc: SemanticDocument): TypeSignature = {
    val signatureTypeList = {
      // TODO: this is horrible
      val signatureShape = term.symbol.info.map(extractParamListsTypes)
      if (signatureShape.exists(_.exists(_.exists(_.isGeneric)))) None
      else signatureShape.map(_.map(_.map(_.asInstanceOf[ConcreteType])))
    }

    val syntheticsTypeList = term.synthetics collect {
      // TODO: cases other than SelectTree - need to make this work for more than just .apply methods
      // same as below case but wrapped in an ApplyTree for some reason - seems to happen with XXX.lift(x, y, z) implicit lift syntax?
      case ApplyTree(TypeApplyTree(SelectTree(_, IdTree(info)), typeArgs), _) =>
        val concreteTypeArgs = typeArgs.map(ConcreteType(_))
        val signatureShape = extractParamListsTypes(info)
        substituteTypes(signatureShape, concreteTypeArgs)
      // method had generic parameters
      case TypeApplyTree(SelectTree(_, IdTree(info)), typeArgs) =>
        val concreteTypeArgs = typeArgs.map(ConcreteType(_))
        val signatureShape = extractParamListsTypes(info) // e.g. List(List(A, Int), List(B)), where some of the types are generic
        substituteTypes(signatureShape, concreteTypeArgs) // substitutes the concrete types in place of the generic ones
      // method had no generic parameters
      case SelectTree(_, IdTree(info)) => extractParamListsTypes(info).asInstanceOf[TypeSignature]

      case TypeApplyTree(_: OriginalTree, typeArgs) =>
        val concreteTypeArgs = typeArgs.map(ConcreteType(_))
        val signatureShape = term.symbol.info.map(extractParamListsTypes)
        substituteTypes(signatureShape.get, concreteTypeArgs) // TODO: handle optional properly
    }
    assert(syntheticsTypeList.length <= 1, s"expected at most one inferred type signature from synthetics, got $syntheticsTypeList")

    signatureTypeList.getOrElse(syntheticsTypeList.headOption.getOrElse(List.empty))
  }

  def substituteTypes(typeSignature: List[List[MethodParamType]], concreteTypes: List[ConcreteType]): TypeSignature = {
    @annotation.tailrec
    def recurse(nested: List[List[MethodParamType]], flat: List[ConcreteType], acc: TypeSignature): TypeSignature = {

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
        case paramList :: next =>
          val (replacedParamList, remainingConcreteTypes) = substituteParamList(paramList, flat, List.empty)
          recurse(next, remainingConcreteTypes, replacedParamList :: acc)
      }
    }

    if (typeSignature.flatten.count(_.isGeneric) != concreteTypes.length) List.empty
    else recurse(typeSignature, concreteTypes, List.empty)
  }
}