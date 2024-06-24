package demo.backup

import parsley.Parsley
import parsley.quick._
import parsley.syntax.zipped._

import demo.lexer.{ident, nat}
import demo.parser._
import parsley.expr.chain

// scalafix:off
// Removed left recursion:
// demo/scalafix FactorLeftRecursion -f parsley-garnish-demo/src/main/scala/demo/backup/parser1.scala
object parser1a {
  private lazy val parser: Parsley[Prog] = prog

  // <prog> ::= <asgns> <expr>
  private lazy val prog: Parsley[Prog] = (asgns, expr).zipped(Prog)
  // <asgns> ::= (<asgn> ';')*
  private lazy val asgns: Parsley[List[Asgn]] = many(asgn <~ char(';'))
  // <asgn> ::= "let" <ident> '=' <expr>
  private lazy val asgn: Parsley[Asgn] = (string("let") ~> ident, char('=') ~> expr).zipped(Asgn)

  // <expr> ::= <expr> '+' <term> | <expr> '-' <term> | <term>
  private lazy val expr: Parsley[Expr] = chain.postfix[Expr](term)((char('+') ~> term).map(x1 => x2 => Add(x2, x1)) | (char('-') ~> term).map(x1 => x2 => Sub(x2, x1)))
  // <term> ::= <term> '*' <atom> | <term> '/' <atom> | <atom>
  private lazy val term: Parsley[Expr] = chain.postfix[Expr](atom)((char('*') ~> atom).map(x1 => x2 => Mul(x2, x1)) | (char('/') ~> atom).map(x1 => x2 => Div(x2, x1)))
  // <atom> ::= <nat> | <ident> | '(' <expr> ')'
  private lazy val atom = nat.map(Val) | ident.map(Var) | char('(') ~> expr <~ char(')')

  def main(args: Array[String]): Unit = {
    println(expr.parse("1+2*3"))
  }
}
