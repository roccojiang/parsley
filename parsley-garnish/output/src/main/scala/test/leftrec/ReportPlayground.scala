package test.leftrec

import parsley.Parsley
import parsley.character._
import parsley.syntax.zipped._
import parsley.expr.chain

object ReportPlayground {
  // lazy val example: Parsley[String] = (example, string("a")).zipped(_ + _) | string("b")
  // ^^^^     ^^^^^^^  ^^^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  // mods      pats        decltpe                             rhs
  lazy val example: Parsley[String] = chain.postfix[String](string("b"))(string("a").map(x1 => x2 => x2 + x1))
}
