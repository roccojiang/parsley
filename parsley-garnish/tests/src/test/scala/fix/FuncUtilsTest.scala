package fix

import org.scalactic.Equality
import org.scalatest.Assertion
import org.scalatest.Inspectors.forAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.meta._
import scala.meta.contrib._
import scala.reflect.ClassTag

import utils.FuncUtils._

class FuncUtilsTest extends AnyFlatSpec with Matchers {
  implicit def termEq[T <: Term : ClassTag] = new Equality[T] {
    def areEqual(a: T, b: Any): Boolean = b match {
      case t: T => a.isEqual(t)
      case _ => false
    }
  }

  // Custom equality objects are not applied recursively, so this has to be defined manually
  // see https://github.com/scalatest/scalatest/issues/917
  implicit val funcArgEq = new Equality[FuncArg] {
    def areEqual(a: FuncArg, b: Any): Boolean = a match {
      case ParameterArg(name) => b.isInstanceOf[ParameterArg] && name.isEqual(b.asInstanceOf[ParameterArg].name)
      case ConcreteArg(arg) => b.isInstanceOf[ConcreteArg] && arg.isEqual(b.asInstanceOf[ConcreteArg].arg)
    }
  }

  "extractParamLists" should "return a method's parameter lists" in {
    val term = q"(x, y) => z => x + y + z"

    val actual = extractParamLists(term)
    val expected = List(List(Term.Name("x"), Term.Name("y")), List(Term.Name("z")))

    assertNestedListsEqual(actual, expected)
  }

  "extractArgs" should "return a method call's arguments" in {
    val term = q"f(x, y)(z)"

    val actual = extractArgs(term)
    val expected = List(List(Term.Name("x"), Term.Name("y")), List(Term.Name("z")))

    assertNestedListsEqual(actual, expected)
  }

  it should "handle more complex arguments" in {
    val term = q"f(g(x, y), h(z))"

    val actual = extractArgs(term)
    val expected = List(List(q"g(x, y)", q"h(z)"))

    assertNestedListsEqual(actual, expected)
  }

  "labelFuncArgs" should "label each method argument as a parameter or concrete value" in {
    val term = q"(a, b) => c => f(a, 3)(b)(2, c)"

    val actual = labelFuncArgs(term)
    val expected = List(List(ParameterArg(Term.Name("a")), ConcreteArg(q"3")), List(ParameterArg(Term.Name("b"))), List(ConcreteArg(q"2"), ParameterArg(Term.Name("c"))))

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
