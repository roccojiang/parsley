package test

import parsley.Parsley, parsley.Parsley._
import parsley.character._
import parsley.combinator._
import parsley.syntax.character._

object SimplifyParserTest {
  def add[A](a: A, b: String): String = a.toString + b

  // shouldn't alpha-convert variables (due to function normalisation) if parsers don't need to be simplified
  val noSimplification = pure((x: Int, y: Int) => x + y)
  val noSimplification2 = pure((_: String) + (_: String))

  /***** Parser laws *****/
  val altRightNeutral = "anise"
  val altLeftNeutral = "coriander"

  val altLeftBiasedChoice = pure("ginger")

  val apRightAbsorb = empty
  val apHomomorphism = pure(1 + 1)
  val fmap = string("rosemary").map((x1: String) => x1 + "thyme")

  val fmapLeftAbsorb = empty

  val fmapHomomorphism = pure(add("parsley", "wasabi"))

  val fmapComposition = string("saffron").map(x1 => add(add(x1, "tarragon"), "turmeric"))

  def defParser(p: Parsley[String]): Parsley[String] = p.map(x1 => add(add(x1, "oregano"), "paprika"))

  /***** Resugaring parsers *****/
  val thenAsAp = "cumin" ~> "dill"
  val thenAsAp2 = string("cumin") ~> "dill"
  val thenDiscardAsAp = string("cumin") <~ "dill"
}
