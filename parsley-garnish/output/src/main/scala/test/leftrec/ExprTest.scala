package test.leftrec

import parsley.Parsley
import parsley.character._
import parsley.generic._
import parsley.expr.chain
import parsley.syntax.zipped._

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
  // TODO: fix compilation issues with expr, term
  lazy val expr: Parsley[Expr] = chain.postfix[Expr](term)((char('+') ~> term).map(hoas611 => hoas612 => Add.curried(hoas612)(hoas611)) | (char('-') ~> term).map(hoas613 => hoas614 => Sub.curried(hoas614)(hoas613)))
  // TODO: we can totally turn this into chain...?
  lazy val term: Parsley[Expr] = chain.postfix[Expr](negate)((char('*') ~> negate).map(hoas841 => hoas842 => Mul.curried(hoas842)(hoas841)))
  lazy val negate: Parsley[Expr] = Neg(string("negate") ~> negate) | atom
  lazy val atom: Parsley[Expr] = (char('(') ~> expr <~ char(')')) | Num(number)

  // lazy val ruleA = chain.postfix[String](string("b").map(hoas677 => hoas678 => hoas677 + hoas678) <*> string("a") | string("a"))(string("b").map(hoas679 => hoas680 => hoas681 => hoas681 + hoas679 + hoas680) <*> string("a"))
  lazy val ruleA = chain.postfix[String]((string("b"), string("a")).zipped((hoas579, hoas580) => hoas579 + hoas580) | string("a"))((string("b"), string("a")).zipped((hoas581, hoas582) => hoas583 => hoas583 + hoas581 + hoas582))
  // lazy val ruleA = chain.postfix[String](string("b").map(hoas677 => hoas678 => hoas677 + hoas678) <*> string("a") | string("a"))((string("b"), string("a")).zipped((hoas679, hoas680) => hoas681 => hoas681 + hoas679 + hoas680))
  lazy val ruleB: Parsley[String] = ruleA.map(a => b => a + b) <*> string("b") | string("b")

  lazy val p: Parsley[String] = chain.postfix[String](string("b"))(string("a").map(hoas21 => hoas22 => hoas22 + hoas21))
}
