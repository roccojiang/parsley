/*
rule = FactorLeftRecursion
 */
package test.leftrec

import parsley.Parsley
import parsley.character._
import parsley.generic._

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
}
