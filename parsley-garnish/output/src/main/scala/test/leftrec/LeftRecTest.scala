package test.leftrec

import parsley.Parsley
import parsley.Parsley._
import parsley.character._
import parsley.expr.chain

object LeftRecTest {
  def flip[A, B, C](f: A => B => C): B => A => C = (b: B) => (a: A) => f(a)(b)

  // TODO: for certain examples like this one with lambdas (or if we can find the function definition to inline?), is it possible to evaluate the flip at compile time?
  // TODO: same thing with if we see pure(identity) as well
  // lazy val p: Parsley[String] = chain.postfix(string("ca"))(pure(identity[String] _).map(((xs: String) => (ba: String) => xs + ba).compose(_)).map(flip(_)) <*> string("ba"))
  lazy val p: Parsley[String] = chain.postfix[String](string("ca"))(string("ba").map(fresh28 => fresh29 => ((xs: String) => (ba: String) => xs + ba)(fresh29)(fresh28)))
  // lazy val p2: Parsley[String] = chain.postfix[String](string("ca"))(string("ba").map(fresh18 => (fresh10 => fresh10)(_) + fresh18))

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

  def main(args: Array[String]): Unit = {
    // val cabaInputs = List("ca", "caba", "cababa")
    // println(cabaInputs.map(pp.parse(_)))

    // val intInputs = List("1", "11", "12", "12345")
    // println(intInputs.map(q.parse(_)))
  }
}
