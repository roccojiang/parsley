package fix

import parsley.Parsley
import parsley.character._
import parsley.syntax.lift._

object SyntheticsTest {
  sealed trait Expr
  case class Add(a: Expr, b: Expr) extends Expr
  case class Atom(a: String) extends Expr

  lazy val r: Parsley[Expr] = chain.postfix[Expr](string("b").map(Atom(_)))(string("a").map(fresh17 => fresh16 => Add(fresh16)((a => Atom(a))(fresh17))))
}
