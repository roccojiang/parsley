/*
rule = FactorLeftRecursion
 */
package test.leftrec

import parsley.Parsley, Parsley._
import parsley.character._
import parsley.syntax.character._
import parsley.syntax.lift._
import parsley.syntax.zipped._
import parsley.expr.chain

object ReportPlayground {
  lazy val example: Parsley[String] = (example, string("a")).zipped(_ + _) | string("b")
  // ^^^^     ^^^^^^^  ^^^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  // mods      pats        decltpe                             rhs

  object introduction {
    val number: Parsley[Float] = digit.foldLeft1(0)((n, d) => n * 10 + d.asDigit).map(_.toFloat)

    lazy val expr: Parsley[Float] = (expr, '+' ~> term).zipped(_ + _) |
                                    (expr, '-' ~> term).zipped(_ - _) |
                                    term
    lazy val term: Parsley[Float] = (term, '*' ~> atom).zipped(_ * _) |
                                    (term, '/' ~> atom).zipped(_ / _) |
                                    atom
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

    lazy val expr: Parsley[Expr] = (expr, '+' ~> term).zipped(Add(_, _)) |
                                   (expr, '-' ~> term).zipped(Sub(_, _)) |
                                   term
    lazy val term: Parsley[Expr] = (term, '*' ~> atom).zipped(Mul(_, _)) |
                                   (term, '/' ~> atom).zipped(Div(_, _)) |
                                   atom
    lazy val atom: Parsley[Expr] = '(' ~> expr <~ ')' | number.map(Num(_))
  }

  object direct {
    trait Expr
    case class Num(n: Int) extends Expr
    case class Inc(x: Expr) extends Expr

    val number: Parsley[Int] = digit.foldLeft1(0)((n, d) => n * 10 + d.asDigit)
    val incs: Parsley[Expr] = Inc.lift(incs) <~ "+" | Num.lift(number)
    val incsByHand: Parsley[Expr] = chain.postfix[Expr](number.map(Num(_)))("+" as Inc)
  }

  object abab {
    // lazy val ruleA = ruleB.map(concat) <*> string("a") | string("a")
    lazy val ruleA = chain.postfix[String]((string("b"), string("a")).zipped((x1, x2) => concat(x1)(x2)) | string("a"))((string("b"), string("a")).zipped((x1, x2) => x3 => concat(x3 + x1)(x2)))
    lazy val ruleB: Parsley[String] = ruleA.map(a => b => a + b) <*> string("b") | string("b")

    def concat(x: String)(y: String): String = x + y
    lazy val ruleAHidden: Parsley[String] = pure(concat) <*> ruleBHidden <*> string("a") | string("a")
    lazy val ruleBHidden: Parsley[String] = pure(concat) <*> ruleAHidden <*> string("b") | string("b")
  }

  object indirect {
    import parsley.generic._

    trait Expr
    case class Num(n: Int) extends Expr
    case class Add(x: Expr, y: Expr) extends Expr
    object Num extends ParserBridge1[Int, Num]
    object Add extends ParserBridge2[Expr, Expr, Add]

    val number: Parsley[Int] = digit.foldLeft1(0)((n, d) => n * 10 + d.asDigit)

    lazy val expr: Parsley[Expr] = add | '(' ~> expr <~ ')' | Num(number)
    lazy val add: Parsley[Expr] = Add(expr, '+' ~> expr)
  }

  def main(args: Array[String]) = {
    println(abab.ruleA.parse("baba"))
  }

  object associativity {
    import parsley.generic._

    trait Expr
    case class Num(n: Int) extends Expr
    case class Add(x: Expr, y: Expr) extends Expr
    object Num extends ParserBridge1[Int, Num]
    object Add extends ParserBridge2[Expr, Expr, Add]

    val number: Parsley[Int] = digit.foldLeft1(0)((n, d) => n * 10 + d.asDigit)
    val term = Num(number)

    lazy val left: Parsley[Expr] = Add(left, '+' ~> term) | term
    lazy val right: Parsley[Expr] = Add(term, '+' ~> right) | term
    lazy val full: Parsley[Expr] = Add(full, '+' ~> full) | term
    // val add: Parsley[Expr] = Add(add, '+' ~> add) | term
    // val add2: Parsley[Expr] = Add(add2, '+' ~> term) | term
    // lazy val term = Num(number)
  }

  object hidden {
    lazy val a1: Parsley[Int] = b1 <*> a1 | digit.map(_.asDigit)/* assert: FactorLeftRecursion
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Left-recursion detected, but could not be removed from a1.
The resulting chain would be given a parser which consumes no input, causing it to loop indefinitely:
chain.postfix[Int](digit.map(x1 => x1.asDigit))(pure(x1 => x1 + 1))
*/
    lazy val b1: Parsley[Int => Int] = pure(_ + 1)

    lazy val a2: Parsley[Int] = b2 ~> a2/* assert: FactorLeftRecursion
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Left-recursion detected, but could not be removed from a2.
The resulting chain would be given a parser which consumes no input, causing it to loop indefinitely:
chain.postfix[Int](some(digit) ~> a2)(pure(x1 => x1))
*/
    lazy val b2: Parsley[Int] = many(digit).map(_.mkString.toInt)
  }
}
