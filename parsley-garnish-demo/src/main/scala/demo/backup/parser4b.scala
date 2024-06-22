package demo.backup

import parsley.Parsley
import parsley.quick._
import parsley.syntax.zipped._
import parsley.expr.chain

import demo._
import demo.lexer.{fully, ident, nat}
import demo.lexer.implicits.implicitSymbol
import parsley.generic._

// Use generic bridges
// demo/scalafix UseGenericBridges -f parsley-garnish-demo/src/main/scala/demo/backup/parser4a.scala
object parser4b {
  case class Prog(decls: List[Asgn], x: Expr)
  case class Asgn(v: String, x: Expr)

  sealed trait Expr
  case class Add(x: Expr, y: Expr) extends Expr
  case class Mul(x: Expr, y: Expr) extends Expr
  case class Val(x: BigInt) extends Expr
  case class Var(v: String) extends Expr

  private lazy val parser: Parsley[Prog] = fully(prog)

  // <prog> ::= <asgns> <expr>
  object Prog extends ParserBridge2[List[Asgn], Expr, Prog]
  private lazy val prog: Parsley[Prog] = Prog(asgns, expr)
  // <asgns> ::= (<asgn> ';')*
  private lazy val asgns: Parsley[List[Asgn]] = endBy(asgn, ";")
  // <asgn> ::= "let" <ident> '=' <expr>
  object Asgn extends ParserBridge2[String, Expr, Asgn]
  private lazy val asgn: Parsley[Asgn] = Asgn("let" ~> ident, "=" ~> expr)

  // <expr> ::= <expr> '+' <term> | <term>
  private lazy val expr: Parsley[Expr] = chain.postfix(term)(("+" ~> term).map(x1 => x2 => Add(x2, x1)))
  // <term> ::= <term> '*' <atom> | <atom>
  private lazy val term: Parsley[Expr] = chain.postfix(atom)(("*" ~> atom).map(x1 => x2 => Mul(x2, x1)))
  // <atom> ::= <nat> | <ident> | '(' <expr> ')'
  object Val extends ParserBridge1[BigInt, Val]
  object Var extends ParserBridge1[String, Var]
  private lazy val atom = Val(nat) | Var(ident) | "(" ~> expr <~ ")"

  def main(args: Array[String]): Unit = {
    println(parser.parse("let x = 1; x + 2"))
  }
}
