/*
rule = FactorLeftRecursion
 */
package test.leftrec

import parsley.Parsley
import parsley.Parsley._
import parsley.character._
import parsley.expr.chain

object LeftRecTest {

  lazy val testMany: Parsley[List[String]] = many(p2) // TODO: figure out a left-recursive parser with many

  def flip[A, B, C](f: A => B => C): B => A => C = (b: B) => (a: A) => f(a)(b)

  // lazy val p: Parsley[String] = pure((xs: String) => (ba: String) => xs + ba) <*> p <*> string("ba") | string("ca")
  lazy val p2: Parsley[String] = p2.map((xs: String) => (ba: String) => xs + ba) <*> string("ba") | string("ca")

  val pl = pure(identity[String] _).map(((xs: String) => (ba: Int) => xs + ba).compose(_))
  val flipped = pl.map(flip(_))

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

  // val testingToday = string("hello", "world")

  // Indirect left recursion with two parsers
  lazy val ruleA = ruleB.map(add) <*> string("a") | string("a")
  lazy val ruleB: Parsley[String] = ruleA.map(add) <*> string("b") | string("b")

  // TODO: should we remove the ruleB definition since it's been inlined into the transformed ruleA?
  // Output:
  // lazy val ruleA2 = chain.postfix(string("b").map(add) <*> string("a") | string("a"))((pure(identity[String] _).map(add.compose(_)).map(flip(_)) <*> string("b")).map(add.compose(_)).map(flip(_)) <*> string("a"))
  // lazy val ruleB2: Parsley[String] = ruleA2.map(add) <*> string("b") | "b"
  
  // val pp = chain.postfix("ca", (pure(identity[String] _).map(((xs: String) => (ba: String) => xs + ba).compose(_)).map(flip(_)) <*> "ba"))

  def main(args: Array[String]): Unit = {
    // val cabaInputs = List("ca", "caba", "cababa")
    // println(cabaInputs.map(pp.parse(_)))

    // val intInputs = List("1", "11", "12", "12345")
    // println(intInputs.map(q.parse(_)))

    // val test = string("p").map(add) <*> empty
  }
}
