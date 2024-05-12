package fix

object A {
  import parsley.syntax.character._

  { 
    import lexer.implicits.implicitSymbol
  }
}

object lexer {
  import parsley.token.Lexer
  import parsley.token.descriptions.LexicalDesc

  private val lexer = new Lexer(LexicalDesc.plain)

  val implicits = lexer.lexeme.symbol.implicits
}

object B { // ok, should not report anything, since the global stringLift is defined below this object
  import lexer.implicits._
}

import parsley.syntax.character.stringLift

object C { 
  import lexer.implicits._

  object D {
    // ok, should not report here even though the clashing implicit conversions are in the parent scope
    // it would be obnoxious to re-report the same issue in every child scope
    import parsley.syntax.character.charLift
  }
}

class E { 
  import parsley.syntax.character._
  import lexer.implicits._
}
