/* 
rule = FactorLeftRecursion
 */
package test

import parsley.quick._
import parsley.syntax.character._
import parsley.syntax.all._

object FactorLeftRecursionTest {
  val number: Parsley[Int] = digit.foldLeft1(0)((n, d) => n * 10 + d.asDigit)

  object DirectUnaryOp {
    trait Expr
    case class Num(n: Int) extends Expr
    case class Inc(x: Expr) extends Expr

    val incsWrong: Parsley[Expr] = Inc.lift(incsWrong) | Num.lift(number)/* assert: FactorLeftRecursion
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Left-recursion detected, but could not be removed from incsWrong.
The resulting chain would be given a parser which consumes no input, causing it to loop indefinitely:
chain.postfix[Expr](number.map(x1 => Num(x1)))(pure(x1 => Inc(x1)))
*/

    val incs: Parsley[Expr] = Inc.lift(incs) <~ "+" | Num.lift(number)
  }

  object DirectExpr {
    trait Expr
    case class Num(n: Int) extends Expr
    case class Add(x: Expr, y: Expr) extends Expr
    case class Sub(x: Expr, y: Expr) extends Expr
    case class Mul(x: Expr, y: Expr) extends Expr
    case class Div(x: Expr, y: Expr) extends Expr

    lazy val expr: Parsley[Expr] = (expr, '+' ~> term).zipped(Add(_, _)) |
                                   (expr, '-' ~> term).zipped(Sub(_, _)) |
                                   term
    lazy val term: Parsley[Expr] = (term, '*' ~> atom).zipped(Mul(_, _)) |
                                   (term, '/' ~> atom).zipped(Div(_, _)) |
                                   atom
    lazy val atom: Parsley[Expr] = '(' ~> expr <~ ')' | number.map(Num(_))
  }

  // TODO: this causes failed compilation due to Scala's inability to infer lambda argument types
  object DirectExprEval {
    val number: Parsley[Float] = digit.foldLeft1(0)((n, d) => n * 10 + d.asDigit).map(_.toFloat)

    lazy val expr: Parsley[Float] = (expr, '+' ~> term).zipped(_ + _) |
                                    (expr, '-' ~> term).zipped(_ - _) |
                                    term
    lazy val term: Parsley[Float] = (term, '*' ~> atom).zipped(_ * _) |
                                    (term, '/' ~> atom).zipped(_ / _) |
                                    atom
    lazy val atom: Parsley[Float] = '(' ~> expr <~ ')' | number
  }

  object IndirectHuttonsRazor {
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

  object IndirectAbabab {
    def concat(x: String)(y: String): String = x + y

    lazy val ruleA = ruleB.map(concat) <*> string("a") | string("a")
    lazy val ruleB: Parsley[String] = ruleA.map(concat) <*> string("b") | string("b")

    // parsley-garnish will desugar to this, so technically it sees it as a case of hidden left-recursion?
    lazy val ruleAHidden: Parsley[String] = pure(concat) <*> ruleBHidden <*> string("a") | string("a")
    lazy val ruleBHidden: Parsley[String] = pure(concat) <*> ruleAHidden <*> string("b") | string("b")
  }

  object Hidden {
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

  object Associativity {
    import parsley.generic._

    trait Expr
    case class Num(n: Int) extends Expr
    case class Add(x: Expr, y: Expr) extends Expr
    object Num extends ParserBridge1[Int, Num]
    object Add extends ParserBridge2[Expr, Expr, Add]

    val term = Num(number)

    lazy val left: Parsley[Expr] = Add(left, '+' ~> term) | term
    lazy val right: Parsley[Expr] = Add(term, '+' ~> right) | term
    lazy val full: Parsley[Expr] = Add(full, '+' ~> full) | term
  }

  object PotentialTodos {
    // TODO: properly consider issues with lazy positions?
    // Will stack overflow for input "a", due to strict positions    
    // lazy val simpleBad: Parsley[String] = string("a") <|> simpleBad
    // Will still infinitely recurse on any input other than "a", obviously
    // lazy val simpleGood: Parsley[String] = string("a") <|> ~simpleGood
  }
}
