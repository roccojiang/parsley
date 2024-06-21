package parsley.garnish

import scala.meta._
import scala.meta.contrib._
import scalafix.v1._

object implicits {
  implicit class TreeOps(private val tree: Tree) extends AnyVal {
    def containsAnyOf(terms: Term*): Boolean = tree.collect {
      case t: Term if terms.toSeq.exists(term => t.isEqual(term)) => t
    }.nonEmpty

    def isWithinScope(scope: Tree): Boolean =
      tree.isEqual(scope) || tree.parent.exists(_.isWithinScope(scope))
  }

  implicit class TermOps(private val term: Term) extends AnyVal {
    import parsley.garnish.expr.{Expr, ExprLifter}
    import parsley.garnish.parser.{Parser, ParserLifter}

    def toParser(implicit doc: SemanticDocument): Parser = ParserLifter.lift(term)
    def toExpr(numParams: Int = 1)(implicit doc: SemanticDocument): Expr = ExprLifter.lift(term, numParams)
  }
}
