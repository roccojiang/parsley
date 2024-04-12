/*
rule = LeftRec
 */
package fix

import parsley.Parsley
import parsley.Parsley._
import parsley.character._
// import parsley.combinator._
// import parsley.expr._
// import parsley.syntax.zipped._
// import parsley.syntax.lift._
import parsley.syntax.character.{charLift, stringLift}

object LeftRecTest {
  val thisIsNotAParsleyValSoItShouldNotBeDetectedAsANonTerminal = List(1) ++ List(2)

  def flip[A, B, C](f: A => B => C): B => A => C = (b: B) => (a: A) => f(a)(b)

  // TODO: for certain examples like this one with lambdas (or if we can find the function definition to inline?), is it possible to evaluate the flip at compile time?
  // TODO: same thing with if we see pure(identity) as well
  lazy val p: Parsley[String] = pure((xs: String) => (ba: String) => xs + ba) <*> p <*> "ba" | string("ca")
  lazy val p2: Parsley[String] = p2.map((xs: String) => (ba: String) => xs + ba) <*> string("ba") | string("ca")

  // TODO: output won't compile due to missing parameter types, need to fix this
  lazy val q: Parsley[Int] = q.map((xs: Int) => (c: Char) => c.asDigit + xs) <*> digit | digit.map(_.asDigit)
  // Correct output:
  // lazy val q: Parsley[Int] = chain.postfix((pure((c: Char) => c.asDigit) <*> digit), (pure(identity[Int] _).map(((xs: Int) => (c: Char) => c.asDigit + xs).compose(_)).map(flip(_)) <*> digit))

  def add(a: String)(b: String) = a + b

  // TODO: handle implicits, so that we can get rid of the string combinator usage
  // TODO: what's going on with the output here?
  lazy val a = b | c
  val b = string("hi")
  lazy val c: Parsley[String] = a.map(add) <*> string("bye")

  // lazy val ac = chain.postfix("hi", (pure(identity[String] _).map((add _).compose(_)).map(flip(_)) <*> "bye"))
  // lazy val cc = chain.postfix((string("hi").map(add) <*> "bye"), (pure(identity[String] _).map((add _).compose(_)).map(flip(_)) <*> "bye"))

  // val test = empty <*> "hello"
  // val test = "hello".as(identity _) <*> empty
  val test = empty.map(a => (b: String) => "bye") <*> string("hello")

  // val pp = chain.postfix("ca", (pure(identity[String] _).map(((xs: String) => (ba: String) => xs + ba).compose(_)).map(flip(_)) <*> "ba"))

  def defParser[A](p: Parsley[A]): Parsley[A] = p

  object NestedScope {
    val nestedParser = p
    val nestedParser2 = nestedParser | a
    
    var varParser = nestedParser
  }
      
  def main(args: Array[String]): Unit = {
    lazy val parser: Parsley[String] = p | parser2
    lazy val parser2 = parser | NestedScope.nestedParser | a

    // val cabaInputs = List("ca", "caba", "cababa")
    // println(cabaInputs.map(pp.parse(_)))

    // val intInputs = List("1", "11", "12", "12345")
    // println(intInputs.map(q.parse(_)))
  }
}
