package demo.backup

import parsley.Parsley
import parsley.quick._
import parsley.syntax.zipped._

import demo.lexer.{ident, nat}
import parsley.expr.chain
import parsley.syntax.character._

// Use implicit conversions:
// demo/scalafix UseImplicitConversions -f parsley-garnish-demo/src/main/scala/demo/backup/parser2.scala
object parser2a {
  case class Prog(decls: List[Asgn], x: Expr)
  case class Asgn(v: String, x: Expr)

  sealed trait Expr
  case class Add(x: Expr, y: Expr) extends Expr
  case class Mul(x: Expr, y: Expr) extends Expr
  case class Val(x: BigInt) extends Expr
  case class Var(v: String) extends Expr

  private lazy val parser: Parsley[Prog] = prog

  // <prog> ::= <asgns> <expr>
  private lazy val prog: Parsley[Prog] = (asgns, expr).zipped(Prog)
  // <asgns> ::= (<asgn> ';')*
  private lazy val asgns: Parsley[List[Asgn]] = many(asgn <~ ';')
  // <asgn> ::= "let" <ident> '=' <expr>
  private lazy val asgn: Parsley[Asgn] = ("let" ~> ident, '=' ~> expr).zipped(Asgn)

  // <expr> ::= <expr> '+' <term> | <term>
  private lazy val expr: Parsley[Expr] = chain.postfix(term)(('+' ~> term).map(x1 => x2 => Add(x2, x1)))
  // <term> ::= <term> '*' <atom> | <atom>
  private lazy val term: Parsley[Expr] = chain.postfix(atom)(('*' ~> atom).map(x1 => x2 => Mul(x2, x1)))
  // <atom> ::= <nat> | <ident> | '(' <expr> ')'
  private lazy val atom = nat.map(Val) | ident.map(Var) | '(' ~> expr <~ ')'

  def main(args: Array[String]): Unit = {
    println(parser.parse("letx=1;x+2"))
  }
}
