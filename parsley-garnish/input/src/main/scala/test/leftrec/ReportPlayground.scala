/*
rule = FactorLeftRecursion
 */
package test.leftrec

import parsley.Parsley, Parsley._
import parsley.character._
import parsley.syntax.zipped._
import parsley.expr.chain

object ReportPlayground {
  // lazy val example: Parsley[String] = (example, string("a")).zipped(_ + _) | string("b")
  // ^^^^     ^^^^^^^  ^^^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  // mods      pats        decltpe                             rhs
  lazy val example: Parsley[String] = (example, string("a")).zipped(_ + _) | string("b")

  // def flip[A, B, C](f: A => B => C)(x: B)(y: A): C = f(y)(x)
  // def compose[A, B, C](f: B => C)(g: A => B)(x: A): C = f(g(x))

  // lazy val expr: Parsley[String] = chain.postfix(
  //   empty | (empty.map((_ + _).curried) | empty <*> expr) <*> string("a")
  //     | string("b") | empty
  // )(
  //   (empty.map(flip) <*> expr | pure(identity).map(compose((_ + _).curried)))
  //     .map(flip) <*> string("a")
  //   | empty | empty
  // )
}
