/*
rule = AmbiguousImplicitConversions
 */
package fix

trait A {
  import parsley.syntax.character.charLift
  import lexer.implicits.implicitSymbol

  // no clashing implicits at this point, since implicitSymbol is only defined for String

  import parsley.syntax.character.stringLift /* assert: AmbiguousImplicitConversions
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
There may be multiple, clashing implicit conversions in this scope:
* import lexer.implicits.implicitSymbol at line 8
* import parsley.syntax.character.stringLift at line 12
If this is the case, you may encounter confusing errors like 'value/method is not a member of String/Char'.
To fix this, ensure that you only import a single implicit conversion.
*/
}

object A {
  import parsley.syntax.character._

  {
    import lexer.implicits.implicitSymbol /* assert: AmbiguousImplicitConversions
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
There may be multiple, clashing implicit conversions in this scope:
* import parsley.syntax.character._ at line 23
* import lexer.implicits.implicitSymbol at line 26
If this is the case, you may encounter confusing errors like 'value/method is not a member of String/Char'.
To fix this, ensure that you only import a single implicit conversion.
*/  
  }
}

object lexer {
  import parsley.token.Lexer
  import parsley.token.descriptions.LexicalDesc

  private val lexer = new Lexer(LexicalDesc.plain)

  val implicits = lexer.lexeme.symbol.implicits
}

object B { 
  import lexer.implicits._ // ok, should not report anything, since the global stringLift is defined below this object
}

import parsley.syntax.character.stringLift

object C {
  import lexer.implicits._ /* assert: AmbiguousImplicitConversions
  ^^^^^^^^^^^^^^^^^^^^^^^^
There may be multiple, clashing implicit conversions in this scope:
* import parsley.syntax.character.stringLift at line 50
* import lexer.implicits._ at line 53
If this is the case, you may encounter confusing errors like 'value/method is not a member of String/Char'.
To fix this, ensure that you only import a single implicit conversion.
*/

  object D {
    // ok, should not report here even though the clashing implicit conversions are in the parent scope
    // it would be obnoxious to re-report the same issue in every child scope
    import parsley.syntax.character.charLift
  }
}

class E {
  import parsley.syntax.character._
  import lexer.implicits._ /* assert: AmbiguousImplicitConversions
  ^^^^^^^^^^^^^^^^^^^^^^^^
There may be multiple, clashing implicit conversions in this scope:
* import parsley.syntax.character.stringLift at line 50
* import parsley.syntax.character._ at line 70
* import lexer.implicits._ at line 71
If this is the case, you may encounter confusing errors like 'value/method is not a member of String/Char'.
To fix this, ensure that you only import a single implicit conversion.
*/
}
