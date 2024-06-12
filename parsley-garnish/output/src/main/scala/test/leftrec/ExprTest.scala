package test.leftrec

import parsley.Parsley, Parsley._
import parsley.character._
import parsley.generic._
import parsley.syntax.lift._
import parsley.syntax.zipped._
import parsley.expr.chain

object ExprTest {
  // Main Expr example is based on the Scala design patterns paper?
  sealed trait Expr
  case class Add(x: Expr, y: Expr) extends Expr
  case class Sub(x: Expr, y: Expr) extends Expr
  case class Mul(x: Expr, y: Expr) extends Expr
  case class Neg(x: Expr) extends Expr
  case class Num(x: Int) extends Expr

  object Add extends ParserBridge2[Expr, Expr, Add]
  object Sub extends ParserBridge2[Expr, Expr, Sub]
  object Mul extends ParserBridge2[Expr, Expr, Mul]
  object Neg extends ParserBridge1[Expr, Neg]
  object Num extends ParserBridge1[Int, Num]

  val number = digit.foldLeft1(0)((n, d) => n * 10 + d.asDigit)
  lazy val expr: Parsley[Expr] = chain.postfix[Expr](term)((char('+') ~> term).map(x1 => x2 => Add.curried(x2)(x1)) | (char('-') ~> term).map(x1 => x2 => Sub.curried(x2)(x1)))
  lazy val term: Parsley[Expr] = chain.postfix[Expr](negate)((char('*') ~> negate).map(x1 => x2 => Mul.curried(x2)(x1)))
  lazy val negate: Parsley[Expr] = Neg(string("negate") ~> negate) | atom
  lazy val atom: Parsley[Expr] = (char('(') ~> expr <~ char(')')) | Num(number)

  lazy val ruleA = chain.postfix[String]((string("b"), string("a")).zipped((x1, x2) => x1 + x2) | string("a"))((string("b"), string("a")).zipped((x1, x2) => x3 => x3 + x1 + x2))
  lazy val ruleB: Parsley[String] = ruleA.map(a => b => a + b) <*> string("b") | string("b")

  lazy val p: Parsley[String] = chain.postfix[String](string("b"))(string("a").map(x1 => x2 => x2 + x1))

  case class Inc(x: Expr) extends Expr
  val incsWrong: Parsley[Expr] = Inc.lift(incsWrong) | Num(number)
  // val incsWrong = chain.postfix[Expr](number.map(Num(_)))(pure(x1 => Inc(x1))) // triggers parsley.exceptions.NonProductiveIterationException
  // TODO: Turn this into example of direct left-recursion? Easy postfix example
  val incs: Parsley[Expr] = chain.postfix[Expr](Num(number))(char('+').map(x1 => x2 => Inc(x2)) | empty)
  // val incs = chain.postfix[Expr](number.map(Num(_)))(char('+').map(x1 => x2 => Inc(x2))) // almost good! except Num needed amending to Num(_) - tagging to turn it back into a bridge apply??

  def main(args: Array[String]): Unit = {
    // println(incs.parse("1+++"))
  }
}
