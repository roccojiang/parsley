package demo

import parsley.Parsley
import parsley.token.{Lexer, predicate}
import parsley.token.descriptions.{LexicalDesc, NameDesc, SpaceDesc, SymbolDesc}

object lexer {
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
