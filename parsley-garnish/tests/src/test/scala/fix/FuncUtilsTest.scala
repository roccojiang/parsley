package fix

import org.scalactic.Equality
import org.scalatest.Assertion
import org.scalatest.Inspectors.forAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.meta._
import scala.meta.contrib._
import scala.reflect.ClassTag

import utils.FuncUtils

class FuncUtilsTest extends AnyFlatSpec with Matchers {
  implicit def termNameEq[T <: Term : ClassTag] = new Equality[T] {
    def areEqual(a: T, b: Any): Boolean = b match {
      case t: T => a.isEqual(t)
      case _ => false
    }
  }

  "extractParamLists" should "return a method's parameter lists" in {
    val term = q"(x, y) => z => x + y + z"

    val actual = FuncUtils.extractParamLists(term)
    val expected = List(List(Term.Name("x"), Term.Name("y")), List(Term.Name("z")))

    assertNestedListsEqual(actual, expected)
  }

  "extractArgs" should "return a method call's arguments" in {
    val term = q"f(x, y)(z)"

    val actual = FuncUtils.extractArgs(term)
    val expected = List(List(Term.Name("x"), Term.Name("y")), List(Term.Name("z")))

    assertNestedListsEqual(actual, expected)
  }

  it should "handle more complex arguments" in {
    val term = q"f(g(x, y), h(z))"

    val actual = FuncUtils.extractArgs(term)
    val expected = List(List(q"g(x, y)", q"h(z)"))

    assertNestedListsEqual(actual, expected)
  }

  private def assertNestedListsEqual[A](actual: List[List[A]], expected: List[List[A]])(implicit eq: Equality[A]): Assertion = {
    actual should have length expected.length.toLong

    forAll (actual.zip(expected)) { case (actualList, expectedList) =>
      actualList should have length expectedList.length.toLong
      forAll (actualList.zip(expectedList)) { case (actualVal, expectedParam) =>
        actualVal should equal (expectedParam)
      } 
    }
  }
}
