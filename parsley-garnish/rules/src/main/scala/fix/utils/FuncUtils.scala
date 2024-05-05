package fix.utils

import scala.meta._
import scalafix.v1._

import TypeUtils.ConcreteType
import Func._

object FuncUtils {
  sealed abstract class FuncArg extends Product with Serializable
  case class ParameterArg(name: Term.Name, tpe: Option[ConcreteType] = None) extends FuncArg
  case class ConcreteArg(arg: Term, tpe: Option[ConcreteType] = None) extends FuncArg

  def extractParamLists(f: Term): List[List[Term.Name]] = f match {
    case Term.Function.After_4_6_0(params, body) => {
      params.values.collect { case Term.Param(_, name: Term.Name, _, _) => name } :: extractParamLists(body)
    }
    // TODO: partial functions? anonymous functions will probably be converted to a proper function by the time we get here
    case _ => List.empty
  }

  def extractArgs(t: Term): List[List[Term]] = {
    def recurse(t: Term): List[List[Term]] = t match {
      case Term.Apply.After_4_6_0(fun, args) => args.values :: recurse(fun)
      case _ => List.empty
    }

    recurse(t).reverse
  }

  // TODO: f: Term.Function ? 
  def labelFuncArgs(f: Term): List[List[FuncArg]] = {
    @annotation.tailrec
    def getFuncBody(f: Term): Term = f match {
      case Term.Function.After_4_6_0(_, body) => getFuncBody(body)
      case _ => f
    }

    val paramNames = extractParamLists(f).flatten.map(_.value).toSet
    val args = extractArgs(getFuncBody(f))

    args.map(_.map(_ match {
      // directly compare with string values, since meta.Trees use reference equality
      case arg: Term.Name if paramNames.contains(arg.value) => ParameterArg(arg)
      case arg => ConcreteArg(arg)
    }))
  }

  def labelFuncArgTypes(args: List[List[FuncArg]], types: List[List[ConcreteType]]): List[List[FuncArg]] = {
    args.zip(types).map { case (args, types) =>
      args.zip(types).map { case (arg, tpe) =>
        arg match {
          case ParameterArg(name, _) => ParameterArg(name, Some(tpe))
          case ConcreteArg(arg, _) => ConcreteArg(arg, Some(tpe))
        }
      }
    }
  }

  def toFunc(f: Opaque, args: List[List[FuncArg]]): Func = {
    // alpha conversion: assign fresh variable names
    val freshArgs = args.map(_.map {
      case ParameterArg(_, tpe) => ParameterArg(Term.fresh(), tpe)
      case arg => arg
    })

    val apps = freshArgs.foldLeft(f: Func) { (acc, args) => {
      val params = args.map {
        case ParameterArg(name, tpe) => Var(name, tpe.map(_.tpe))
        case ConcreteArg(arg, _)     => Opaque(arg)
      }
      App(acc, params: _*)
    }}

    freshArgs.foldRight(apps) { (args, acc) => {
      val params = args.collect {
        case ParameterArg(name, tpe) => Var(name, tpe.map(_.tpe))
      }
      if (params.isEmpty) acc else Lam(params, acc)
    }}
  }
}
