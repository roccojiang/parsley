package parsley.garnish.model

import scala.meta._

import Expr._
import Parser._

object resugaring {
  def bridgeApply: PartialFunction[Parser, Parser] = {
    // this is so ridiculously broken it's not even funny, I don't know why it's triggering incorrectly so often
    case FMap(p, f @ Translucent(Term.Name(_), _)) => Bridge(f, List(p))
    // case FMap(p1, f) <*> p2 => Bridge(f, List(p1, p2))
  }

  def thenResugarer: PartialFunction[Parser, Parser] = {
    case FMap(p, Abs(_, Abs(x, y))) <*> q if (x == y) => p ~> q
  }

  private def foldAps(p: Parser): List[Parser] = p match {
    case p <*> q => foldAps(p) :+ q
    case _ => List(p)
  }
}
