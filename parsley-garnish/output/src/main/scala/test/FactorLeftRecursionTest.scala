package test

import parsley.quick._
import parsley.syntax.character._
import parsley.syntax.all._
import parsley.expr.chain

object FactorLeftRecursionTest {
  val number: Parsley[Int] = digit.foldLeft1(0)((n, d) => n * 10 + d.asDigit)

  object DirectUnaryOp {
    trait Expr
    case class Num(n: Int) extends Expr
    case class Inc(x: Expr) extends Expr

    val incsWrong: Parsley[Expr] = Inc.lift(incsWrong) | Num.lift(number)

    val incs: Parsley[Expr] = chain.postfix[Expr](number.map(x1 => Num(x1)))(string("+").map(x1 => x2 => Inc(x2)))
  }

  object DirectExpr {
    trait Expr
    case class Num(n: Int) extends Expr
    case class Add(x: Expr, y: Expr) extends Expr
    case class Sub(x: Expr, y: Expr) extends Expr
    case class Mul(x: Expr, y: Expr) extends Expr
    case class Div(x: Expr, y: Expr) extends Expr

    lazy val expr: Parsley[Expr] = chain.postfix[Expr](term)(('+' ~> term).map(x1 => x2 => Add(x2, x1)) | ('-' ~> term).map(x1 => x2 => Sub(x2, x1)))
    lazy val term: Parsley[Expr] = chain.postfix[Expr](atom)(('*' ~> atom).map(x1 => x2 => Mul(x2, x1)) | ('/' ~> atom).map(x1 => x2 => Div(x2, x1)))
    lazy val atom: Parsley[Expr] = '(' ~> expr <~ ')' | number.map(Num(_))
  }

  // TODO: this causes failed compilation due to Scala's inability to infer lambda argument types
  object DirectExprEval {
    val number: Parsley[Float] = digit.foldLeft1(0)((n, d) => n * 10 + d.asDigit).map(_.toFloat)

    lazy val expr: Parsley[Float] = chain.postfix[Float](term)(('+' ~> term).map(x1 => x2 => x2 + x1) | ('-' ~> term).map(x1 => x2 => x2 - x1))
    lazy val term: Parsley[Float] = chain.postfix[Float](atom)(('*' ~> atom).map(x1 => x2 => x2 * x1) | ('/' ~> atom).map(x1 => x2 => x2 / x1))
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

    lazy val expr: Parsley[Expr] = chain.postfix[Expr]('(' ~> expr <~ ')' | number.map(x1 => Num(x1)))(('+' ~> expr).map(x1 => x2 => Add(x2, x1)))
    lazy val add: Parsley[Expr] = Add(expr, '+' ~> expr)
  }

  object IndirectAbabab {
    def concat(x: String)(y: String): String = x + y

    lazy val ruleA = chain.postfix[String]((string("b"), string("a")).zipped((x1, x2) => concat(x1)(x2)) | string("a"))((string("b"), string("a")).zipped((x1, x2) => x3 => concat(concat(x3)(x1))(x2)))
    lazy val ruleB: Parsley[String] = ruleA.map(concat) <*> string("b") | string("b")

    // parsley-garnish will desugar to this, so technically it sees it as a case of hidden left-recursion?
    lazy val ruleAHidden: Parsley[String] = chain.postfix[String]((string("b"), string("a")).zipped((x1, x2) => concat(x1)(x2)) | string("a"))((string("b"), string("a")).zipped((x1, x2) => x3 => concat(concat(x3)(x1))(x2)))
    lazy val ruleBHidden: Parsley[String] = pure(concat) <*> ruleAHidden <*> string("b") | string("b")
  }

  object Hidden {
    lazy val a1: Parsley[Int] = b1 <*> a1 | digit.map(_.asDigit)
    lazy val b1: Parsley[Int => Int] = pure(_ + 1)

    lazy val a2: Parsley[Int] = b2 ~> a2
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

    lazy val left: Parsley[Expr] = chain.postfix[Expr](term)(('+' ~> term).map(x1 => x2 => Add(x2, x1)))
    lazy val right: Parsley[Expr] = Add(term, '+' ~> right) | term
    lazy val full: Parsley[Expr] = chain.postfix[Expr](term)(('+' ~> full).map(x1 => x2 => Add(x2, x1)))
  }

  object PotentialTodos {
    // TODO: properly consider issues with lazy positions?
    // Will stack overflow for input "a", due to strict positions    
    // lazy val simpleBad: Parsley[String] = string("a") <|> simpleBad
    // Will still infinitely recurse on any input other than "a", obviously
    // lazy val simpleGood: Parsley[String] = string("a") <|> ~simpleGood
  }
}
