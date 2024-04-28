package fix.utils

import scalafix.v1.SymbolMatcher

object Matchers {
  val parsley = SymbolMatcher.normalized("parsley.Parsley")

  // TODO: update this
  val zipped = SymbolMatcher.normalized(
    (0 to 22).map(i => s"parsley.implicits.zipped.Zipped$i#zipped"): _*
  )

  val liftExplicit = SymbolMatcher.normalized(
    (1 to 22).map(i => s"parsley.lift.lift$i"): _*
  )
  val liftImplicit = SymbolMatcher.normalized(
    (0 to 22).map(i => s"parsley.syntax.lift.Lift$i#lift"): _*
  )

  val string = SymbolMatcher.normalized("parsley.character.string")
  val digit = SymbolMatcher.normalized("parsley.character.digit")
  val map = SymbolMatcher.normalized("parsley.Parsley.map")
  val empty = SymbolMatcher.normalized("parsley.Parsley.empty")
  val pure = SymbolMatcher.normalized("parsley.Parsley.pure")
  val <*> = SymbolMatcher.normalized("parsley.Parsley.`<*>`")
  val <|> = SymbolMatcher.normalized("parsley.Parsley.`|`", "parsley.Parsley.`<|>`")
}
