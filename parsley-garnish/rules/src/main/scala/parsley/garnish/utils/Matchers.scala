package parsley.garnish.utils

import scalafix.v1.SymbolMatcher

object Matchers {
  val parsley = SymbolMatcher.normalized("parsley.Parsley")
  
  val liftExplicit = SymbolMatcher.normalized(
    (1 to 22).map(i => s"parsley.lift.lift$i"): _*
  )
  val liftImplicit = SymbolMatcher.normalized(
    (0 to 22).map(i => s"parsley.syntax.lift.Lift$i#lift"): _*
  )
  val zipped = SymbolMatcher.normalized(
    (2 to 22).map(i => s"parsley.syntax.zipped.Zipped$i#zipped"): _*
  )
  val bridgeApply = SymbolMatcher.normalized(
    (1 to 22).map(i => s"parsley.generic.ParserBridge$i#apply"): _*
  )

  val string = SymbolMatcher.normalized("parsley.character.string")
  val digit = SymbolMatcher.normalized("parsley.character.digit")
  val map = SymbolMatcher.normalized("parsley.Parsley.map")
  val empty = SymbolMatcher.normalized("parsley.Parsley.empty")
  val pure = SymbolMatcher.normalized("parsley.Parsley.pure")
  val many = SymbolMatcher.normalized("parsley.Parsley.many")
  val some = SymbolMatcher.normalized("parsley.Parsley.some")
  val <*> = SymbolMatcher.normalized("parsley.Parsley.`<*>`")
  val <|> = SymbolMatcher.normalized("parsley.Parsley.`|`", "parsley.Parsley.`<|>`")
}
