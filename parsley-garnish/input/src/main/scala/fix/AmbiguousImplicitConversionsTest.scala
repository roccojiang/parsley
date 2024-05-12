/*
rule = AmbiguousImplicitConversions
 */
package fix

object A {
  import parsley.syntax.character._
  object B { /* assert: AmbiguousImplicitConversions
  ^
There may be multiple, clashing implicit conversions in this scope:
* import lexer.implicits.implicitSymbol at line 16
* import parsley.syntax.character._ at line 7
If this is the case, you may encounter confusing errors like 'value/method is not a member of String/Char'.
To fix this, ensure that you only import a single implicit conversion.
*/
  import lexer.implicits.implicitSymbol
  }
}

object lexer {
  import parsley.Parsley
  import parsley.token.{Lexer, predicate}
  import parsley.token.descriptions._

  private val lexer = new Lexer(LexicalDesc.plain.copy(
    nameDesc = NameDesc.plain.copy(
      identifierStart = predicate.Basic(_.isLetter),
      identifierLetter = predicate.Basic(_.isLetterOrDigit),
    ),
    symbolDesc = SymbolDesc.plain.copy(
      hardKeywords = Set("neg"),
    ),
  ))

  def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)

  val ident = lexer.lexeme.names.identifier
  val implicits = lexer.lexeme.symbol.implicits
}

object C {  // ok, should not report anything, since the global stringLift is defined below this object
  import lexer.implicits._
}

import parsley.syntax.character.stringLift

object D {  /* assert: AmbiguousImplicitConversions
^
There may be multiple, clashing implicit conversions in this scope:
* import lexer.implicits._ at line 55
* import parsley.syntax.character.stringLift at line 45
If this is the case, you may encounter confusing errors like 'value/method is not a member of String/Char'.
To fix this, ensure that you only import a single implicit conversion.
*/
  import lexer.implicits._
}

object parser {
  import parsley.expr.chain
  import lexer.{fully, ident}

  import parsley.errors.combinator.ErrorMethods
  import parsley.syntax.character.stringLift
  import lexer.implicits._

  /*
  trait Expr
  case class Var(v: String) extends Expr
  case class Neg(e: Expr) extends Expr

  val atom = ident.map(Var)
  val expr = chain.prefix[Expr](atom)("neg" as Neg) // scala2 compiler can report the names of the conflicting implicits
  
  val prog = fully(expr)

  val ha = "hello" ~> "world"

  // .label is one of the combinators that is still defined by extension method since the Parsley 4.x series
  // the scala2 compiler is unable to report the names of the conflicting implicits here, only reporting that 'label' is not a member of String
  // interestingly, scala3 can report the conflicting implicits though
  val ohNo = "oh dear".label("oh no")

  def main(args: Array[String]): Unit = {
    println(prog.parse("neg x"))
    println(prog.parse("negx"))
    println(prog.parse("neg negx"))
  }
  */

  def main(args: Array[String]): Unit = {
    println("hello world")
    import parsley.character.string
  }
}
