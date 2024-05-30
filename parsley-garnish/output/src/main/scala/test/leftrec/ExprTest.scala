package test.leftrec

import parsley.Parsley
import parsley.character._
import parsley.generic._
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

  lazy val expr: Parsley[Expr] = chain.postfix[Expr](term.map(Mul.apply.curried) <*> char('*') ~> negate | ((string("negate") ~> negate).map(Neg.apply) | (char('(') ~> expr <~ char(')') | digit.foldLeft1(0)((n, d) => n * 10 + d.asDigit).map(Num.apply))))((char('+') ~> term).map(hoas535 => (hoas536: Expr) => Add.apply.curried(hoas536)(hoas535)) | (char('-') ~> term).map(hoas537 => (hoas538: Expr) => Sub.apply.curried(hoas538)(hoas537)))
  lazy val term: Parsley[Expr] = chain.postfix[Expr]((string("negate") ~> negate).map(Neg.apply) | (char('(') ~> expr <~ char(')') | digit.foldLeft1(0)((n, d) => n * 10 + d.asDigit).map(Num.apply)))((char('*') ~> negate).map(hoas680 => hoas681 => Mul.apply.curried(hoas681)(hoas680)))
  lazy val negate: Parsley[Expr] = Neg(string("negate") ~> negate) | atom
  lazy val atom: Parsley[Expr] = (char('(') ~> expr <~ char(')')) | Num(number)

  def add(a: String)(b: String) = a + b
  lazy val ruleA = ruleB.map(add) <*> string("a") | string("a")
  lazy val ruleB: Parsley[String] = ruleA.map(add) <*> string("b") | string("b")
}
