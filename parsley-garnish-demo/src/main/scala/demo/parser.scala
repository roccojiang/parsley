package demo

import parsley.Parsley
import parsley.quick._

case class Prog(decls: List[Asgn], x: Expr)
case class Asgn(v: String, x: Expr)

sealed trait Expr
case class Add(x: Expr, y: Expr) extends Expr
case class Mul(x: Expr, y: Expr) extends Expr
case class Val(x: BigInt) extends Expr
case class Var(v: String) extends Expr

object parser {
  private lazy val parser: Parsley[Prog] = prog

  // <prog> ::= <asgns> <expr>
  private lazy val prog = ???
  // <asgns> ::= (<asgn> ';')*
  private lazy val asgns = pure(List.empty[Asgn])
  // <asgn> ::= "let" <ident> '=' <expr>
  private lazy val asgn = ???

  // <expr> ::= <expr> '+' <term> | <term>
  private lazy val expr: Parsley[Expr] = term
  // <term> ::= <term> '*' <atom> | <atom>
  private lazy val term: Parsley[Expr] = atom
  // <atom> ::= <nat> | <ident> | '(' <expr> ')'
  private lazy val atom = ???
}
