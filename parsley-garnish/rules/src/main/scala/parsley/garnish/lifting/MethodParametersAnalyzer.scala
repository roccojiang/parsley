package parsley.garnish.lifting

import scala.meta._

import TypeSignatureAnalyzer.{ConcreteType, TypeSignature}

object MethodParametersAnalyzer {

  sealed abstract class FuncArgument extends Product with Serializable

  case class ParameterArg(name: Term.Name, tpe: Option[ConcreteType] = None) extends FuncArgument

  case class ConcreteArg(arg: Term, tpe: Option[ConcreteType] = None) extends FuncArgument

  private[lifting] def extractParamLists(f: Term): List[List[Term.Name]] = f match {
    case Term.Function.After_4_6_0(params, body) =>
      params.values.collect { case Term.Param(_, name: Term.Name, _, _) => name } :: extractParamLists(body)
    case _ => List.empty
  }

  private[lifting] def extractArgs(t: Term): List[List[Term]] = {
    def recurse(t: Term): List[List[Term]] = t match {
      case Term.Apply.After_4_6_0(fun, args) => args.values :: recurse(fun)
      case _ => List.empty
    }

    recurse(t).reverse
  }

  def getFuncArguments(f: Term): List[List[FuncArgument]] = {
    @annotation.tailrec
    def getFuncBody(f: Term): Term = f match {
      case Term.Function.After_4_6_0(_, body) => getFuncBody(body)
      case _ => f
    }

    val paramNames = extractParamLists(f).flatten.map(_.value).toSet
    val args = extractArgs(getFuncBody(f))

    args.map(_.map {
      // directly compare with string values, since meta.Trees use reference equality
      case arg: Term.Name if paramNames.contains(arg.value) => ParameterArg(arg)
      case arg => ConcreteArg(arg)
    })
  }

  /** Attempts to match each argument with its corresponding type in the given type signature.
    * If the shape of the type signature does not match up, the list of arguments takes precedence,
    * and they will be returned without having their types updated.
   */
  def updateFuncArgTypes(argsLists: List[List[FuncArgument]], typeSignature: TypeSignature): List[List[FuncArgument]] = {
    if (argsLists.length != typeSignature.length) argsLists
    else {
      argsLists.zip(typeSignature).map { case (args, types) =>
        if (args.length != types.length) args
        else {
          args.zip(types).map { case (arg, tpe) =>
            arg match {
              case ParameterArg(name, _) => ParameterArg(name, Some(tpe))
              case ConcreteArg(arg, _) => ConcreteArg(arg, Some(tpe))
            }
          }
        }
      }
    }
  }
}
