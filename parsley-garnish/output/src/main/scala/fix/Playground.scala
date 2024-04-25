package fix

import parsley.Parsley
import parsley.Parsley._
import parsley.character._
import parsley.debug._

import parsley.syntax.lift._
import parsley.lift._
import parsley.expr.chain

object Playground {
  // case class Add(a: Int, b: Int)
  // val p = Add.lift(string("a") as 1, string("b") as 2)
  // val q = lift2(Add(_, _), string("a") as 1, string("b") as 2)

  sealed trait Expr
  case class Add(a: Expr, b: Expr) extends Expr
  case class Atom(a: String) extends Expr
  // val p = Add.lift(string("a").map(Atom(_)), Atom.lift(string("b")))
  // val q = lift2(Add(_, _), string("a"), string("b"))

  lazy val r: Parsley[Expr] = Add.lift(r, string("a").map(Atom(_))) | string("b").map(Atom(_))

  def flip[A, B, C](f: A => B => C): B => A => C = (b: B) => (a: A) => f(a)(b)

  // lazy val rManual: Parsley[Expr] = chain.postfix(string("b").map(Atom(_)))(p => Add.lift(p, string("a").map(Atom(_))))

   case class Inc(x: Expr) extends Expr
   case class Dec(x: Expr) extends Expr
   case class Num(x: Int) extends Expr
   val expr = chain.postfix(digit.map(d => Num(d.asDigit)))(string("++").as(Inc(_)))

  // lazy val r2: Parsley[Expr] = chain.postfix(string("b").map(Atom(_)))(pure(identity[Expr] _).map(Add.curried.compose(_)).map(flip(_)) <*> string("a").map(Atom(_)))


  val add = pure(identity[Expr] _).map(Add.curried.compose(_))
  val add2 = pure(Add.curried)

  val flipped = add.map(flip(_))
  // val add = Add.curried.compose(identity[Expr] _)


  // lazy val r: Parsley[Expr] = chain.postfix(string("b").map(Atom(_)))((pure(identity[Expr] _).map(flip(_)) <*> string("a").map(Atom(_))).map(Add.compose(_)))
  // lazy val r2: Parsley[Expr] = chain.postfix(string("b").map(Atom(_)))(a.map(d))

  val b = pure(identity[Expr => Expr]).map(flip(_))
  val a = b <*> string("a").map(Atom(_))

  val c: (Expr => Expr) => (Expr => (Expr => Expr)) = (param: Expr => Expr) => Add.curried.compose(param)

  val d: ((Expr => Expr) => Expr) => (Expr => Expr) = ???

  
  def main(args: Array[String]): Unit = {
    def add(a: String)(b: String) = a + b
    
    val input = "parse"

    val test = ((string("p").map(add)).debug("p") <*> empty).debug("test")

    val parsed = test.parse(input)
    println(parsed)


    val test2 = (empty.debug("empty") <*> string("p").debug("p")).debug("test2")
    println(test2.parse(input))
  }
}
