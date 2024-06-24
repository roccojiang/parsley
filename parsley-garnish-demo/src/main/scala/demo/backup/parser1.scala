package demo.backup

import parsley.Parsley
import parsley.quick._
import parsley.syntax.zipped._

import demo.lexer.{ident, nat}
import demo.parser._

// scalafix:off
object parser1 {
  private lazy val parser: Parsley[Prog] = prog

  // <prog> ::= <asgns> <expr>
  private lazy val prog: Parsley[Prog] = (asgns, expr).zipped(Prog)
  // <asgns> ::= (<asgn> ';')*
  private lazy val asgns: Parsley[List[Asgn]] = many(asgn <~ char(';'))
  // <asgn> ::= "let" <ident> '=' <expr>
  private lazy val asgn: Parsley[Asgn] = (string("let") ~> ident, char('=') ~> expr).zipped(Asgn)

  // <expr> ::= <expr> '+' <term> | <expr> '-' <term> | <term>
  private lazy val expr: Parsley[Expr] = (expr, char('+') ~> term).zipped(Add) |
                                         (expr, char('-') ~> term).zipped(Sub) |
                                         term
  // <term> ::= <term> '*' <atom> | <term> '/' <atom> | <atom>
  private lazy val term: Parsley[Expr] = (term, char('*') ~> atom).zipped(Mul) |
                                         (term, char('/') ~> atom).zipped(Div) |
                                         atom
  // <atom> ::= <nat> | <ident> | '(' <expr> ')'
  private lazy val atom = nat.map(Val) | ident.map(Var) | char('(') ~> expr <~ char(')')

  def main(args: Array[String]): Unit = {
    println(expr.parse("1+2*3"))
  }
}
