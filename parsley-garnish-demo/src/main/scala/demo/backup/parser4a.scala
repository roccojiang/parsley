package demo.backup

import parsley.Parsley
import parsley.quick._
import parsley.syntax.zipped._
import parsley.expr.chain

import demo._
import demo.lexer.{fully, ident, nat}
import demo.lexer.implicits.implicitSymbol

// Avoid parser redefinitions
// demo/scalafix AvoidParserRedefinition -f parsley-garnish-demo/src/main/scala/demo/backup/parser4.scala
object parser4a {
  private lazy val parser: Parsley[Prog] = fully(prog)

  // <prog> ::= <asgns> <expr>
  private lazy val prog: Parsley[Prog] = (asgns, expr).zipped(Prog)
  // <asgns> ::= (<asgn> ';')*
  private lazy val asgns: Parsley[List[Asgn]] = endBy(asgn, ";")
  // <asgn> ::= "let" <ident> '=' <expr>
  private lazy val asgn: Parsley[Asgn] = ("let" ~> ident, "=" ~> expr).zipped(Asgn)

  // <expr> ::= <expr> '+' <term> | <term>
  private lazy val expr: Parsley[Expr] = chain.postfix(term)(("+" ~> term).map(x1 => x2 => Add(x2, x1)))
  // <term> ::= <term> '*' <atom> | <atom>
  private lazy val term: Parsley[Expr] = chain.postfix(atom)(("*" ~> atom).map(x1 => x2 => Mul(x2, x1)))
  // <atom> ::= <nat> | <ident> | '(' <expr> ')'
  private lazy val atom = nat.map(Val) | ident.map(Var) | "(" ~> expr <~ ")"

  def main(args: Array[String]): Unit = {
    println(parser.parse("let x = 1; x + 2"))
  }
}
