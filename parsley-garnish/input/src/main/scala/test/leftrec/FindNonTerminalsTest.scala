/*
rule = FactorLeftRecursion
FactorLeftRecursion.debugOptions = [reportNonTerminalLocations]
 */
package test.leftrec

import parsley.Parsley
import parsley.Parsley.empty
import parsley.character.string

// TODO: refactor this to be decoupled from FactorLeftRecursion rule
/**
  * Unit tests for the detection of non-terminals in order to perform auto-factoring of left recursion.
  * 
  * Each `// assert: FactorLeftRecursion` comment asserts that the corresponding parser should be detected as a non-terminal.
  */
object FindNonTerminalsTest {
  // sbt garnishTests/testOnly *RuleSuite -- -z FindNonTerminalsTest

  val notAParser = List(1) ++ List(2)

  val p: Parsley[String] = string("thyme") /* assert: FactorLeftRecursion
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
p was detected to be a non-terminal, and parsed as:
Str(thyme,false)
*/

  var q = string("rosemary") // assert: FactorLeftRecursion
  lazy val r: Parsley[Unit] = empty.void // assert: FactorLeftRecursion

  def add(a: String)(b: String) = a + b
  val s = p.map(add) <*> string("sage") // assert: FactorLeftRecursion

  def t[A](p: Parsley[A]): Parsley[(A, String)] = p <~> q // TODO: I don't believe this should be detected as a non-terminal?

  // These should probably be rare in practice, so they aren't supported for now
  /*
  val u, v = p        // TODO: declaring multiple fields in one line, this currently can cause conflicts with overlapping patches
  val (x, y) = (p, q) // tuple unpacking won't be detected
  */

  object NestedScope {
    val p = r <~> s // assert: FactorLeftRecursion
  }

  def main(args: Array[String]): Unit = {
    // sbt garnishInput/run

    val p = t(s) // assert: FactorLeftRecursion
    println(p.parse("thymesagerosemary"))
  }
}
