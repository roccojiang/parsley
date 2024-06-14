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
      import parsley.syntax.character.stringLift
      val strLift = "parsley" <~> string("garnish")
      import parsley.syntax.character.charLift
      val chrLift = 'p' <~> string("arsley")
    }

    object PartiallyQualified {
      import parsley.syntax
      import parsley.syntax.character

      import syntax.character.stringLift
      val strLift = "parsley" <~> string("garnish")
      import character.charLift
      val chrLift = 'p' <~> string("arsley")
    }

    object Imported {
      import parsley.syntax.character._

      val strLift = "parsley" <~> string("garnish")
      val chrLift = 'p' <~> string("arsley")
    }
  }

  object lexerSyms {
    object FullyQualified {
      import lexer.implicits.implicitSymbol
      val implicitSym = "parsley" <~> string("garnish")
    }

    object PartiallyQualified {
      import lexer.implicits

      import implicits.implicitSymbol
      val implicitSym = "parsley" <~> string("garnish")
    }

    object Imported {
      import lexer.implicits._

      val implicitSym = "parsley" <~> string("garnish")
    }
  }
}
