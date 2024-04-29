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
  // TODO: try a lift3 as below
  case class Wow(a: Expr, b: Expr, c: Expr) extends Expr
  case class Atom(a: String) extends Expr
  // val p = Add.lift(string("a").map(Atom(_)), Atom.lift(string("b")))
  // val q = lift2(Add(_, _), string("a"), string("b"))

  lazy val r: Parsley[Expr] = chain.postfix[Expr](string("b").map(Atom(_)))(string("a").map(Atom(_)).map(flip(Add.curried)))

  lazy val s: Parsley[Expr] = chain.postfix[Expr](string("c").map(Atom(_)))(string("a").map(Atom(_)).map(flip(Wow.curried)).map(flip(_)) <*> string("b").map(Atom(_)))

  // lazy val s2: Parsley[Expr] = chain.postfix(string("c").map(Atom(_)))((pure(identity[Expr] _).map(Wow.compose(_)).map(flip(_)) <*> string("a").map(Atom(_))).map(flip(_)) <*> string("b").map(Atom(_)))
  lazy val s2: Parsley[Expr] = chain.postfix[Expr](string("c").map(Atom(_)))((pure(identity[Expr] _).map(Wow.curried.compose(_)).map(flip(_)) <*> string("a").map(Atom(_))).map(flip(_)) <*> string("b").map(Atom(_)))

  def flip[A, B, C](f: A => B => C): B => A => C = (b: B) => (a: A) => f(a)(b)

  
  // TODO: try in Scala 3 to see if type inference still needs nudge, ask Jamie???
  case class Inc(x: Expr) extends Expr
  case class Num(x: Int) extends Expr
  val expr = chain.postfix[Expr](digit.map(d => Num(d.asDigit)))(string("++").as(Inc(_)))
  
  // TODO: THE TRICK FOR MAKING TYPE INFERENCE HAPPY BY PLACING postfix[Expr] HERE
  // TODO: need to curry the Add constructor
  // lazy val r: Parsley[Expr] = chain.postfix(string("b").map(Atom(_)))(pure(identity[Expr] _).map(Add.compose(_)).map(flip(_)) <*> string("a").map(Atom(_)))
  lazy val r2 = chain.postfix[Expr](string("b").map(Atom(_)))(pure(identity[Expr] _).map(Add.curried.compose(_)).map(flip(_)) <*> string("a").map(Atom(_)))
  
  // TODO: figure out how to convert r2 to rManual - definition of as = this *> pure(x)  or  this.map(_ => x)
  lazy val rManual = chain.postfix[Expr](string("b").map(Atom(_)))(string("a").as(Add(_, Atom("a"))))


  val add1 = pure(identity[Expr] _).map(Add.curried.compose(_))
  val add2 = pure(Add.curried)
  val flipped = add2.map(flip(_))

  val flipped2 = pure(flip(Add.curried))


  def addL(a: List[String])(b: String) = a.mkString + b
  // This should be infinitely diverging!
  val manyTest = chain.postfix(string("b"))(many(string("a")).map(addL))


  // Can't be fixed with postfix
  lazy val simpleBad: Parsley[String] = chain.postfix[String](string("a"))(pure(identity[String])) // will stack overflow for input "a", due to strict positions
  lazy val simpleGood: Parsley[String] = string("a") | ~simpleGood // will still infinitely recurse on any input other than "a", obviously

  lazy val simpleBadPostfix: Parsley[String] = chain.postfix(string("a"))(pure(identity))

  def add(a: String)(b: String) = a + b
  lazy val infiniteAs: Parsley[String] = string("a").map(add) <*> infiniteAs // not left rec: (Empty, Ap(FMap(Str(a),add),NonTerminal(infiniteAs)), Empty)
  
  lazy val useless: Parsley[String] = chain.postfix[String](empty)(pure(identity[String]).map((_ => "a").compose(_)))
  // lazy val useless2: Parsley[String] = chain.postfix[String](empty)(pure(identity[String] _).map((_: String => "a").compose(_))) // === chain.postfix(empty)(pure(_ => "a"))
 
  def main(args: Array[String]): Unit = {
    println(simpleBadPostfix.parse("aaa"))
    
    // val input = "parse"

    // val test = ((string("p").map(add)).debug("p") <*> empty).debug("test")

    // val parsed = test.parse(input)
    // println(parsed)


    // val test2 = (empty.debug("empty") <*> string("p").debug("p")).debug("test2")
    // println(test2.parse(input))

    // val test = (empty.debug("empty").map(add).debug("add")).debug("whole")
    // println(test.parse("hello"))
    // val test2 = empty.debug("empty")
    // println(test2.parse("hello"))

    // val parse = s2.parse("cababab")
    // println(parse)
  }
}
