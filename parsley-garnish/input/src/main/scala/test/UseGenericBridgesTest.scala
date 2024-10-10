/* 
rule = UseGenericBridges
 */
package test

import parsley.Parsley
import parsley.combinator.endBy
import parsley.expr.chain
import parsley.syntax.zipped._

object UseGenericBridgesTest {
  case class Prog(decls: List[Asgn], x: Expr)
  case class Asgn(v: String, x: Expr)

  sealed trait Expr
  case class Add(x: Expr, y: Expr) extends Expr
  case class Mul(x: Expr, y: Expr) extends Expr
  case class Val(x: BigInt) extends Expr
  case class Var(v: String) extends Expr

  object lexer {
    import parsley.token.{Lexer, predicate}
    import parsley.token.descriptions.{LexicalDesc, NameDesc, SpaceDesc, SymbolDesc}
    
    private val desc = LexicalDesc.plain.copy(
      nameDesc = NameDesc.plain.copy(
        identifierStart = predicate.Basic(_.isLetter),
        identifierLetter = predicate.Basic(_.isLetterOrDigit),
      ),
      spaceDesc = SpaceDesc.plain,
      symbolDesc = SymbolDesc.plain.copy(
        hardKeywords = Set("let"),
        hardOperators = Set("+", "*"),
      ),
    )
    private val lexer = new Lexer(desc)

    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
    val ident: Parsley[String] = lexer.lexeme.names.identifier
    val nat: Parsley[BigInt] = lexer.lexeme.unsigned.decimal

    val implicits = lexer.lexeme.symbol.implicits
  }

  object parser {
    import lexer._, lexer.implicits.implicitSymbol

    private lazy val parser: Parsley[Prog] = fully(prog)

    // <prog> ::= <asgns> <expr>
    private lazy val prog: Parsley[Prog] = (asgns, expr).zipped(Prog)
    // <asgns> ::= (<asgn> ';')*
    private lazy val asgns: Parsley[List[Asgn]] = endBy(asgn, ";")
    // <asgn> ::= "let" <ident> '=' <expr>
    // TODO: improve type inference so the types to Asgn can be retrieved
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
}
