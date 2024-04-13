/*
rule = FactorLeftRecursion
FactorLeftRecursion.debugOptions = [reportNonTerminalLocations]
 */
package fix

import parsley.Parsley
import parsley.Parsley._
import parsley.character._

/* Unit tests for the detection of non-terminals in order to perform auto-factoring of left recursion. */
object FindNonTerminalsTest {
  // sbt garnishTests/testOnly *RuleSuite -- -z FindNonTerminalsTest

  val notAParser = List(1) ++ List(2)

  val p = string("hello") // assert: FactorLeftRecursion
  var q = string("goodbye") // assert: FactorLeftRecursion
  lazy val r: Parsley[Unit] = ~r.void // assert: FactorLeftRecursion

  def add(a: String)(b: String) = a + b
  val s = p.map(add) <*> string("bye") // assert: FactorLeftRecursion

  def t[A](p: Parsley[A]): Parsley[(A, String)] = p <~> q // assert: FactorLeftRecursion

  object NestedScope {
    val p = r <~> s // assert: FactorLeftRecursion
  }

  def main(args: Array[String]): Unit = {
    // sbt garnishInput/run

    val p = t(s) // assert: FactorLeftRecursion
    println(p.parse("hellobyegoodbye"))
  }
}
