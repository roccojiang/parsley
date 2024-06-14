/* 
rule = NoExplicitImplicitConversions
 */
package test

import parsley.character.string

object NoExplicitImplicitConversionsTest {
  object lexer {
    import parsley.token.Lexer
    import parsley.token.descriptions.LexicalDesc

    private val lexer = new Lexer(LexicalDesc.plain)

    val implicits = lexer.lexeme.symbol.implicits
  }

  object charSyntaxLifts {
    object FullyQualified {
      val strLift = parsley.syntax.character.stringLift("parsley") <~> string("garnish")
      val chrLift = parsley.syntax.character.charLift('p') <~> string("arsley")
    }

    object PartiallyQualified {
      import parsley.syntax
      import parsley.syntax.character

      val strLift = syntax.character.stringLift("parsley") <~> string("garnish")
      val chrLift = character.charLift('p') <~> string("arsley")
    }

    object Imported {
      import parsley.syntax.character._

      val strLift = stringLift("parsley") <~> string("garnish")
      val chrLift = charLift('p') <~> string("arsley")
    }
  }

  object lexerSyms {
    object FullyQualified {
      val implicitSym = lexer.implicits.implicitSymbol("parsley") <~> string("garnish")
    }

    object PartiallyQualified {
      import lexer.implicits

      val implicitSym = implicits.implicitSymbol("parsley") <~> string("garnish")
    }

    object Imported {
      import lexer.implicits._

      val implicitSym = implicitSymbol("parsley") <~> string("garnish")
    }
  }
}
