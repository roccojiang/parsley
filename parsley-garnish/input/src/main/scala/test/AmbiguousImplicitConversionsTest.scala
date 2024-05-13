/*
rule = AmbiguousImplicitConversions
 */
package test

class Anise {
  def anise(): Unit = {
    import parsley.syntax.character._, lexer.implicits._ /* assert: AmbiguousImplicitConversions
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
This import may cause multiple, clashing implicit conversions:
* import parsley.syntax.character._, lexer.implicits._ at line 8
If this is the case, you may encounter confusing errors like 'value/method is not a member of String/Char'.
To fix this, ensure that you only import a single implicit conversion.
*/
  }
}

trait Basil {
  import parsley.syntax.character.charLift
  import lexer.implicits.implicitSymbol

  // no clashing implicits at this point, since implicitSymbol is only defined for String

  import parsley.syntax.character.stringLift /* assert: AmbiguousImplicitConversions
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
This import may cause multiple, clashing implicit conversions:
* import lexer.implicits.implicitSymbol at line 20
* import parsley.syntax.character.stringLift at line 24
If this is the case, you may encounter confusing errors like 'value/method is not a member of String/Char'.
To fix this, ensure that you only import a single implicit conversion.
*/
}

object Clove {
  import parsley.syntax.character._

  {
    import lexer.implicits.implicitSymbol /* assert: AmbiguousImplicitConversions
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
This import may cause multiple, clashing implicit conversions:
* import parsley.syntax.character._ at line 35
* import lexer.implicits.implicitSymbol at line 38
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

object Dill {
  import lexer.implicits._ // ok, should not report anything, since the global stringLift is defined below this object
}

import parsley.syntax.character.stringLift

object Epazote {
  import lexer.implicits._ /* assert: AmbiguousImplicitConversions
  ^^^^^^^^^^^^^^^^^^^^^^^^
This import may cause multiple, clashing implicit conversions:
* import parsley.syntax.character.stringLift at line 62
* import lexer.implicits._ at line 65
If this is the case, you may encounter confusing errors like 'value/method is not a member of String/Char'.
To fix this, ensure that you only import a single implicit conversion.
*/

  object Fennel {
    // ok, should not report here even though the clashing implicit conversions are in the parent scope
    // it would be obnoxious to re-report the same issue in every child scope
    import parsley.syntax.character.charLift
  }
}

class Garlic {
  import parsley.syntax.character._
  import lexer.implicits._ /* assert: AmbiguousImplicitConversions
  ^^^^^^^^^^^^^^^^^^^^^^^^
This import may cause multiple, clashing implicit conversions:
* import parsley.syntax.character.stringLift at line 62
* import parsley.syntax.character._ at line 82
* import lexer.implicits._ at line 83
If this is the case, you may encounter confusing errors like 'value/method is not a member of String/Char'.
To fix this, ensure that you only import a single implicit conversion.
*/
}
