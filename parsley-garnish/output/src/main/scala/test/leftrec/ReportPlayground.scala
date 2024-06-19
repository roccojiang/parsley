package test.leftrec

import parsley.Parsley, Parsley._
import parsley.character._
import parsley.syntax.character._
import parsley.syntax.lift._
import parsley.syntax.zipped._
import parsley.expr.chain

object ReportPlayground {
  lazy val example: Parsley[String] = chain.postfix[String](string("b"))(string("a").map(x1 => x2 => x2 + x1))
  // ^^^^     ^^^^^^^  ^^^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  // mods      pats        decltpe                             rhs

  object introduction {
    val number: Parsley[Float] = digit.foldLeft1(0)((n, d) => n * 10 + d.asDigit).map(_.toFloat)

    lazy val expr: Parsley[Float] = chain.postfix[Float](term)(('+' ~> term).map(x1 => x2 => x2 + x1) | ('-' ~> term).map(x1 => x2 => x2 - x1))
    lazy val term: Parsley[Float] = chain.postfix[Float](atom)(('*' ~> atom).map(x1 => x2 => x2 * x1) | ('/' ~> atom).map(x1 => x2 => x2 / x1))
    lazy val atom: Parsley[Float] = '(' ~> expr <~ ')' | number
  }

  object background {
    trait Expr
    case class Num(n: Int) extends Expr
    case class Add(x: Expr, y: Expr) extends Expr
    case class Sub(x: Expr, y: Expr) extends Expr
    case class Mul(x: Expr, y: Expr) extends Expr
    case class Div(x: Expr, y: Expr) extends Expr

    val number: Parsley[Int] = digit.foldLeft1(0)((n, d) => n * 10 + d.asDigit)

    lazy val expr: Parsley[Expr] = chain.postfix[Expr](term)(('+' ~> term).map(x1 => x2 => Add(x2, x1)) | ('-' ~> term).map(x1 => x2 => Sub(x2, x1)))
    lazy val term: Parsley[Expr] = chain.postfix[Expr](atom)(('*' ~> atom).map(x1 => x2 => Mul(x2, x1)) | ('/' ~> atom).map(x1 => x2 => Div(x2, x1)))
    lazy val atom: Parsley[Expr] = '(' ~> expr <~ ')' | number.map(Num(_))

    val test = ('+' ~> term).map(x1 => x2 => Add(x2, x1))
  }

  object direct {
    trait Expr
    case class Num(n: Int) extends Expr
    case class Inc(x: Expr) extends Expr

    val number: Parsley[Int] = digit.foldLeft1(0)((n, d) => n * 10 + d.asDigit)
    val incs: Parsley[Expr] = chain.postfix[Expr](number.map(x1 => Num(x1)))(string("+").map(x1 => x2 => Inc(x2)))
    // val incsByHand: Parsley[Expr] = chain.postfix[Expr](number.map(Num(_)))("+" as Inc)
  }

  object indirect {
    import parsley.generic._

    trait Expr
    case class Num(n: Int) extends Expr
    case class Add(x: Expr, y: Expr) extends Expr
    object Num extends ParserBridge1[Int, Num]
    object Add extends ParserBridge2[Expr, Expr, Add]

    val number: Parsley[Int] = digit.foldLeft1(0)((n, d) => n * 10 + d.asDigit)

    lazy val expr: Parsley[Expr] = chain.postfix[Expr]('(' ~> expr <~ ')' | number.map(x1 => Num(x1)))(('+' ~> expr).map(x1 => x2 => Add(x2, x1)))
    lazy val add: Parsley[Expr] = Add(expr, '+' ~> expr)
  }

  object fullyAssoc {
    import parsley.generic._

    trait Expr
    case class Num(n: Int) extends Expr
    case class Add(x: Expr, y: Expr) extends Expr
    object Num extends ParserBridge1[Int, Num]
    object Add extends ParserBridge2[Expr, Expr, Add]

    val number: Parsley[Int] = digit.foldLeft1(0)((n, d) => n * 10 + d.asDigit)
    val add: Parsley[Expr] = chain.postfix[Expr](number.map(x1 => Num(x1)))(('+' ~> add).map(x1 => x2 => Add(x2, x1)))
  }

  object hidden {
    lazy val a1: Parsley[Int] = b1 <*> a1 | digit.map(_.asDigit)
    lazy val b1: Parsley[Int => Int] = pure(_ + 1)

    lazy val a2: Parsley[Int] = b2 ~> a2
    lazy val b2: Parsley[Int] = many(digit).map(_.mkString.toInt)
  }
}
