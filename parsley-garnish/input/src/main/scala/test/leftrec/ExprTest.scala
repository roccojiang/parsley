/*
rule = FactorLeftRecursion
// FactorLeftRecursion.debugOptions = [reportNonTerminalLocations]
 */
package test.leftrec

import parsley.Parsley, Parsley._
import parsley.character._
import parsley.generic._
import parsley.syntax.lift._
import parsley.syntax.zipped._
import parsley.expr.chain

object ExprTest {
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

  val number = digit.foldLeft1(0)(((n, d) => n * 10 + d.asDigit))
  lazy val expr: Parsley[Expr] = Add(expr, char('+') ~> term) | Sub(expr, char('-') ~> term) | term
  lazy val term: Parsley[Expr] = Mul(term, char('*') ~> negate) | negate
  lazy val negate: Parsley[Expr] = Neg(string("negate") ~> negate) | atom
  lazy val atom: Parsley[Expr] = (char('(') ~> expr <~ char(')')) | Num(number)

  lazy val ruleA = ruleB.map(a => b => a + b) <*> string("a") | string("a")
  lazy val ruleB: Parsley[String] = ruleA.map(a => b => a + b) <*> string("b") | string("b")

  lazy val p: Parsley[String] = (p, string("a")).zipped(_ + _) | string("b")

  case class Inc(x: Expr) extends Expr
  val incsWrong: Parsley[Expr] = Inc.lift(incsWrong) | Num(number)
  // val incsWrong = chain.postfix[Expr](number.map(Num(_)))(pure(x1 => Inc(x1))) // triggers parsley.exceptions.NonProductiveIterationException
  val incs: Parsley[Expr] = Inc.lift(incs) <~ char('+') | Num(number)

  val inc3 = chain.postfix[Expr](number.map(Num(_)))(char('+').map(x1 => x2 => Inc(x2)))

  def main(args: Array[String]): Unit = {
// [error] parsley.exceptions.NonProductiveIterationException: chain given parser which consumes no input, this will cause it to loop indefinitely
// [error]         at test.leftrec.ExprTest$.main(ExprTest.scala:44)
// [error]         at test.leftrec.ExprTest.main(ExprTest.scala)
// [error]         at java.base/jdk.internal.reflect.DirectMethodHandleAccessor.invoke(DirectMethodHandleAccessor.java:103)
// [error]         at java.base/java.lang.reflect.Method.invoke(Method.java:580)
    println(inc3.parse("1+++"))
  }
}
