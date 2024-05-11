/*
rule = AmbiguousImplicitConversions
 */
package fix

object A {
  import parsley.syntax.character._ // !!!
  object B {
    import lexer.implicits.implicitSymbol // !!!
  }
}

import scala.collection.mutable

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

import scala.language.implicitConversions  

object parser {
  import parsley.expr.chain
  import lexer.{fully, ident}

  import parsley.errors.combinator.ErrorMethods
  import parsley.syntax.character._
  import lexer.implicits.implicitSymbol

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
