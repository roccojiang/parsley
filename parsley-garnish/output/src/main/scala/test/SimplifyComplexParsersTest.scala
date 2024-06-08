package test

import parsley.Parsley, parsley.Parsley._
import parsley.character._
import parsley.combinator._
import parsley.syntax.character._

object SimplifyComplexParsersTest {
  def add[A](a: A, b: String): String = a.toString + b

  val altRightNeutral = "anise"
  val altLeftNeutral = "coriander"

  val altLeftBiasedChoice = pure("ginger")

  val apRightAbsorb = empty
  val apHomomorphism = pure(1 + 1)
  val fmap = "rosemary".map((_l2: String) => _l2 + "thyme") // TODO: this doesn't compile

  val fmapLeftAbsorb = empty

  val fmapHomomorphism = pure(add("parsley", "wasabi"))

  val fmapComposition = string("saffron").map(_x11 => add(add(_x11, "tarragon"), "turmeric"))

  def defParser(p: Parsley[String]): Parsley[String] = p.map(_x14 => add(add(_x14, "oregano"), "paprika"))
}
