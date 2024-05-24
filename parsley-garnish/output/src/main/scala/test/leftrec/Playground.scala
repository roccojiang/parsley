package test.leftrec

import parsley.Parsley
import parsley.Parsley._
import parsley.character._
import parsley.expr.chain
import parsley.syntax.lift._
import parsley.generic._

object Playground {
  // case class Add(a: Int, b: Int)
  // val p = Add.lift(string("a") as 1, string("b") as 2)
  // val q = lift2(Add(_, _), string("a") as 1, string("b") as 2)

  sealed trait Expr
  case class Add(a: Expr, b: Expr) extends Expr
  // TODO: try a lift3 as below
  case class Wow(a: Expr, b: Expr, c: Expr) extends Expr
  case class Atom(a: String) extends Expr

  case class AddBridged(a: Expr, b: Expr) extends Expr
  object AddBridged extends ParserBridge2[Expr, Expr, AddBridged]
  // val p = Add.lift(string("a").map(Atom(_)), Atom.lift(string("b")))
  // val q = lift2(Add(_, _), string("a"), string("b"))

  lazy val r: Parsley[Expr] = chain.postfix[Expr](string("b").map(Atom(_)))(string("a").map(fresh72 => fresh69 => Add.curried(fresh69)(Atom(fresh72))))

  lazy val r2: Parsley[Expr] = chain.postfix[Expr](string("b").map(Atom(_)))(string("a").map(fresh97 => fresh94 => AddBridged.curried(fresh94)(Atom(fresh97))))

  lazy val s: Parsley[Expr] = chain.postfix[Expr](string("c").map(Atom(_)))(string("a").map(fresh39 => fresh35 => fresh36 => Wow.curried(fresh36)(Atom(fresh39))(fresh35)) <*> string("b").map(Atom(_)))
 
  // TODO: figure out how to convert r2 to rManual - definition of as = this *> pure(x)  or  this.map(_ => x)
  // lazy val rManual = chain.postfix[Expr](string("b").map(Atom(_)))(string("a").as(Add(_, Atom("a"))))
  // lazy val rManual2 = chain.postfix[Expr](string("b").map(Atom(_)))(string("a").map(s => Add(_, Atom(s)))) // .map(s => e => Add(e, Atom(s)))

  def addL(a: List[String])(b: String) = a.mkString + b
  // This should be infinitely diverging!
  val manyTest = chain.postfix(string("b"))(many(string("a")).map(addL))

  // Can't be fixed with postfix
  // TODO: make sure these stay unchanged, report a lint error
  lazy val simpleBad: Parsley[String] = chain.postfix[String](string("a"))(pure(fresh38 => fresh38)) // will stack overflow for input "a", due to strict positions
  lazy val simpleGood: Parsley[String] = string("a") | ~simpleGood // will still infinitely recurse on any input other than "a", obviously

  lazy val simpleBadPostfix: Parsley[String] = chain.postfix(string("a"))(pure(identity))

  def add(a: String)(b: String) = a + b
  lazy val infiniteAs: Parsley[String] = string("a").map(add) <*> infiniteAs // not left rec: (Empty, Ap(FMap(Str(a),add),NonTerminal(infiniteAs)), Empty)
  
  lazy val useless: Parsley[String] = chain.postfix[String](empty)(pure(fresh45 => (_ => "a")(fresh45)))
  // lazy val useless2: Parsley[String] = chain.postfix[String](empty)(pure(identity[String] _).map((_: String => "a").compose(_))) // === chain.postfix(empty)(pure(_ => "a"))
 
  def main(args: Array[String]): Unit = {
    //
  }
}
