package test.leftrec

import parsley.Parsley, Parsley._
import parsley.character._
import parsley.generic._
import parsley.syntax.lift._
import parsley.syntax.zipped._
import parsley.expr.chain
import parsley.syntax.character.stringLift

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
  lazy val expr: Parsley[Expr] = chain.postfix[Expr](term)((char('+') ~> term).map(x1 => x2 => Add(x2, x1)) | (char('-') ~> term).map(x1 => x2 => Sub(x2, x1)))
  lazy val term: Parsley[Expr] = chain.postfix[Expr](negate)((char('*') ~> negate).map(x1 => x2 => Mul(x2, x1)))
  lazy val negate: Parsley[Expr] = Neg(string("negate") ~> negate) | atom
  lazy val atom: Parsley[Expr] = (char('(') ~> expr <~ char(')')) | Num(number)

  lazy val ruleA = chain.postfix[String]((string("b"), string("a")).zipped((x1, x2) => x1 + x2) | string("a"))((string("b"), string("a")).zipped((x1, x2) => x3 => x3 + x1 + x2))
  lazy val ruleB: Parsley[String] = ruleA.map(a => b => a + b) <*> string("b") | string("b")

  lazy val p: Parsley[String] = chain.postfix[String](string("b"))(string("a").map(x1 => x2 => x2 + x1))

  case class Inc(x: Expr) extends Expr
  // triggers parsley.exceptions.NonProductiveIterationException if factored out
  val incsWrong: Parsley[Expr] = Inc.lift(incsWrong) | Num(number)
  // TODO: example for direct left-recursion
  val incs: Parsley[Expr] = chain.postfix[Expr](number.map(x1 => Num(x1)))(char('+').map(x1 => x2 => Inc(x2)))

  // ambiguous - this becomes right-associative?
  // val add: Parsley[Expr] = Add(add <~ "+", add) | Num(number)
  // val add: Parsley[Expr] = chain.postfix[Expr](Num(number))(string("+").map(x1 => (x2: Expr) => (x3: Expr) => Add.apply.curried(x3)(x2)) <*> add)
  // TODO: example for non-associative weirdness?
  val add: Parsley[Expr] = chain.postfix[Expr](number.map(x1 => Num(x1)))(("+" ~> add).map(x1 => x2 => Add(x2, x1)))
  // OLD OUTPUT = chain.postfix[Expr](Num(number))(("+" ~> add).map(x1 => x2 => Add.apply.curried(x2)(x1))) // yessss fixed the appending curried issue by case 3 of inferring params of Term.Names

  case class Pair(x: Expr, y: Expr) extends Expr
  object Pair extends ParserBridge2[Expr, Expr, Pair]

  case class Var(v: String) extends Expr
  case class VarB(v: String) extends Expr
  object VarB extends ParserBridge1[String, VarB]

  object varBridge {
    lazy val indirectA: Parsley[Expr] = chain.postfix[Expr]((string("ho"), VarB("hi")).zipped((x1, x2) => Pair(VarB(x1), x2)))(VarB("hi").map(x1 => x2 => Pair(x2, x1)))
    lazy val indirectB = indirectA | VarB("ho")
    // lazy val indirectB = chain.postfix[Expr](string("ho").map(x1 => VarB(x1)))(VarB("hi").map(x1 => x2 => Pair(x2, x1)))
  }
  object varLift {
    lazy val indirectA: Parsley[Expr] = chain.postfix[Expr]((string("ho"), Var.lift("hi")).zipped((x1, x2) => Pair(Var(x1), x2)))(Var.lift("hi").map(x1 => x2 => Pair(x2, x1)))
    lazy val indirectB = indirectA | Var.lift("ho")
  }
  // lazy val indirectA: Parsley[Expr] = chain.postfix[Expr](string("ho").map(x1 => Pair.curried(Var(x1))) <*> Var.lift("hi"))(Var.lift("hi").map(x1 => x2 => Pair.curried(x2)(x1))) // doesn't work well if Var is a bridge
  // lazy val indirectA: Parsley[Expr] = chain.postfix[Expr](string("ho").map(x1 => Pair.curried(Var(x1))) <*> Var.lift("hi"))(Var.lift("hi").map(x1 => x2 => Pair(x2, x1)))
  //                                                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ uncurrying to zipped can't trigger here, because "Pair.curried(Var(x1))" is translucent, so we can't tell that it's got form "x1 => x2 => body"

  // chain p op = postfix p (flip <$> op <*> p)

  // val hidden = ???
  // val derivesEmpty = pure()

  def main(args: Array[String]): Unit = {
    // println(incs.parse("1+++"))
    // val test = "1+2*3+4-5"
    // println(expr.parse(test))
    // println(add.parse("1+2+3+4"))
    println(varLift.indirectA.parse("hohihihi"))
  }
}
