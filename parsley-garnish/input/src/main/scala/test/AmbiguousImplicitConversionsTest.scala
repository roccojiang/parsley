/*
rule = AmbiguousImplicitConversions
 */
package test

object AmbiguousImplicitConversionsTest {
  class Anise {
    def anise(): Unit = {
      import parsley.syntax.character._, lexer.implicits._ /* assert: AmbiguousImplicitConversions
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
This import may cause clashing implicit conversions:
* import parsley.syntax.character._, lexer.implicits._ at line 9
If this is the case, you may encounter confusing errors like 'method is not a member of String'.
To fix this, ensure that you only import a single implicit conversion.
*/

      // val badParser = "anise" ~> "basil" // expected error: value ~> is not a member of String
    }
  }

  trait Basil {
    import parsley.syntax.character.charLift
    import lexer.implicits.implicitSymbol

    // no clashing implicits at this point, since implicitSymbol is only defined for String
    val parser = "basil" ~> "clove"

    import parsley.syntax.character.stringLift /* assert: AmbiguousImplicitConversions
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
This import may cause clashing implicit conversions:
* import lexer.implicits.implicitSymbol at line 23
* import parsley.syntax.character.stringLift at line 28
If this is the case, you may encounter confusing errors like 'method is not a member of String'.
To fix this, ensure that you only import a single implicit conversion.
*/

    // val badParser = "basil" <~ "dill" // expected error: value <~ is not a member of String
  }

  object Clove {
    import parsley.syntax.character._

    {
      import lexer.implicits.implicitSymbol /* assert: AmbiguousImplicitConversions
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
This import may cause clashing implicit conversions:
* import parsley.syntax.character._ at line 41
* import lexer.implicits.implicitSymbol at line 44
If this is the case, you may encounter confusing errors like 'method is not a member of String'.
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
This import may cause clashing implicit conversions:
* import parsley.syntax.character.stringLift at line 68
* import lexer.implicits._ at line 71
If this is the case, you may encounter confusing errors like 'method is not a member of String'.
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
This import may cause clashing implicit conversions:
* import parsley.syntax.character.stringLift at line 68
* import parsley.syntax.character._ at line 88
* import lexer.implicits._ at line 89
If this is the case, you may encounter confusing errors like 'method is not a member of String'.
To fix this, ensure that you only import a single implicit conversion.
*/
  }
}
