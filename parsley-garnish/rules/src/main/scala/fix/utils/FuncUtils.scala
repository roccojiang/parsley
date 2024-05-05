package fix.utils

import scala.meta._
import scalafix.v1._

object FuncUtils {
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
}
