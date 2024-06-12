package parsley.garnish.model

import Parser._

object resugaring {
  def bridgeApply: PartialFunction[Parser, Parser] = {
    case apChain: Ap =>
      println("FOLDED %%%%%%%%%%" + foldAps(apChain))
      apChain
    case FMap(p, f) => Bridge(f, List(p))
    // case FMap(p1, f) <*> p2 => Bridge(f, List(p1, p2))
  }

  private def foldAps(p: Parser): List[Parser] = p match {
    case p <*> q => foldAps(p) :+ q
    case _ => List(p)
  }
}
