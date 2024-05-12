/*
rule = AmbiguousImplicitConversions
 */
package fix

object A {
  import parsley.syntax.character._

  { /* assert: AmbiguousImplicitConversions
  ^
There may be multiple, clashing implicit conversions in this scope:
* import parsley.syntax.character._ at line 7
* import lexer.implicits.implicitSymbol at line 17
If this is the case, you may encounter confusing errors like 'value/method is not a member of String/Char'.
To fix this, ensure that you only import a single implicit conversion.
*/
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

object C { /* assert: AmbiguousImplicitConversions
         ^
There may be multiple, clashing implicit conversions in this scope:
* import parsley.syntax.character.stringLift at line 34
* import lexer.implicits._ at line 44
If this is the case, you may encounter confusing errors like 'value/method is not a member of String/Char'.
To fix this, ensure that you only import a single implicit conversion.
*/
  import lexer.implicits._

  object D {
    // ok, should not report here even though the clashing implicit conversions are in the parent scope
    // it would be obnoxious to re-report the same issue in every child scope
    import parsley.syntax.character.charLift
  }
}

class E { /* assert: AmbiguousImplicitConversions
        ^
There may be multiple, clashing implicit conversions in this scope:
* import parsley.syntax.character.stringLift at line 34
* import parsley.syntax.character._ at line 62
* import lexer.implicits._ at line 63
If this is the case, you may encounter confusing errors like 'value/method is not a member of String/Char'.
To fix this, ensure that you only import a single implicit conversion.
*/
  import parsley.syntax.character._
  import lexer.implicits._
}
