/*
rule = FactorLeftRecursion
 */
package test.leftrec

import parsley.Parsley
import parsley.character._
import parsley.syntax.zipped._
import parsley.expr.chain

object ReportPlayground {
  // lazy val example: Parsley[String] = (example, string("a")).zipped(_ + _) | string("b")
  // ^^^^     ^^^^^^^  ^^^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  // mods      pats        decltpe                             rhs
  lazy val example: Parsley[String] = (example, string("a")).zipped(_ + _) | string("b")
}
