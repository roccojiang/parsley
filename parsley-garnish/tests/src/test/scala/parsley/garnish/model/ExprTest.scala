package parsley.garnish.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.meta._

import Expr._

class ExprTest extends AnyFlatSpec with Matchers {
  "Nested translucent expressions" should "splice correctly" in {
    val expr = Translucent(q"_l1 + _l2", Map("_l1" -> Translucent(q"_l3 + _l4", Map("_l3" -> Var("x3", None), "_l4" -> Var("x1", None))), "_l2" -> Var("x2", None)))
    val expected = "x3 + x1 + x2"

    expr.normalise.term.syntax shouldEqual expected
  }
}
