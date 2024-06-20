package parsley.garnish.model.parser

import scalafix.v1.SymbolMatcher

object matchers {
  /* Core parsers */
  val pure = SymbolMatcher.normalized("parsley.ParsleyImpl.pure")
  val empty = SymbolMatcher.normalized("parsley.ParsleyImpl.empty")
  val choice = SymbolMatcher.normalized("parsley.Parsley.`|`", "parsley.Parsley.`<|>`")
  val ap = SymbolMatcher.normalized("parsley.Parsley.`<*>`")

  /* Lift parsers */
  val map = SymbolMatcher.normalized("parsley.Parsley.map")
  val liftExplicit = SymbolMatcher.normalized(
    (1 to 22).map(i => s"parsley.lift.lift$i"): _*
  )
  val liftImplicit = SymbolMatcher.normalized(
    (0 to 22).map(i =>
      Seq(s"parsley.syntax.Lift$i#lift", s"parsley.syntax.lift.liftSyntax$i")
    ).flatten: _*
  )
  val zipped = SymbolMatcher.normalized(
    (2 to 22).map(i =>
      Seq(s"parsley.syntax.Zipped$i#zipped", s"parsley.syntax.zipped.zippedSyntax$i")
    ).flatten: _*
  )
  val bridge = SymbolMatcher.normalized(
    (1 to 22).map(i => s"parsley.generic.ParserBridge$i#apply"): _*
  )

  /* Character parsers */
  val stringLift = SymbolMatcher.normalized("parsley.syntax.character.stringLift")
  val string = SymbolMatcher.normalized("parsley.character.string")
  val charLift = SymbolMatcher.normalized("parsley.syntax.character.charLift")
  val char = SymbolMatcher.normalized("parsley.character.char")

  /* Sequencing parsers */
  val `then` = SymbolMatcher.normalized("parsley.Parsley.`~>`", "parsley.Parsley.`*>`")
  val thenDiscard = SymbolMatcher.normalized("parsley.Parsley.`<~`", "parsley.Parsley.`<*`")

  /* Chaining parsers */
  // TODO: postfix

  /* Iterative parsers */
  val many = SymbolMatcher.normalized("parsley.ParsleyImpl.many")
  val some = SymbolMatcher.normalized("parsley.ParsleyImpl.some")

  /* Separated values parsers */
  val endBy = SymbolMatcher.normalized("parsley.combinator.endBy")
}
