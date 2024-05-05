package fix.utils

import scala.meta._
import scalafix.v1._

object FuncUtils {
  sealed abstract class FuncArg extends Product with Serializable
  case class ParameterArg(name: Term.Name) extends FuncArg
  case class ConcreteArg(arg: Term) extends FuncArg

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

  def labelFuncArgs(f: Term.Function): List[List[FuncArg]] = {
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
}
