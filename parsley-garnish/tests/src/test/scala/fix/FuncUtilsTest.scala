package fix

import org.scalactic.Equality
import org.scalatest.Assertion
import org.scalatest.Inspectors.forAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.meta._
import scala.meta.contrib._

import utils.FuncUtils

class FuncUtilsTest extends AnyFlatSpec with Matchers {
  // TODO: this doesn't work with Term, but T <: Term suffers from type erasure, use ClassTags if we need to generalise?
  implicit val termNameEq = new Equality[Term.Name] {
    def areEqual(a: Term.Name, b: Any): Boolean = b match {
      case t: Term.Name => a.isEqual(t)
      case _ => false
    }
  }

  "extractParamLists" should "return a method's parameter lists" in {
    val term = q"(x, y) => z => x + y + z"

    val actual = FuncUtils.extractParamLists(term)
    val expected = List(List(Term.Name("x"), Term.Name("y")), List(Term.Name("z")))

    assertParamListsEqual(actual, expected)
  }

  private def assertParamListsEqual(actual: List[List[Term.Name]], expected: List[List[Term.Name]]): Assertion = {
    actual should have length expected.length.toLong

    forAll (actual.zip(expected)) { case (actualParamList, expectedParamList) =>
      actualParamList should have length expectedParamList.length.toLong
      forAll (actualParamList.zip(expectedParamList)) { case (actualParam, expectedParam) =>
        actualParam should equal (expectedParam)
      } 
    }
  }
}
