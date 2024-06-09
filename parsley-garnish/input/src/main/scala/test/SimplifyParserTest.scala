/* 
rule = SimplifyParser
 */
package test

import parsley.Parsley, parsley.Parsley._
import parsley.character._
import parsley.combinator._
import parsley.syntax.character._

object SimplifyParserTest {
  def add[A](a: A, b: String): String = a.toString + b

  // shouldn't alpha-convert variables (due to function normalisation) if parsers don't need to be simplified
  val noSimplification = pure((x: Int, y: Int) => x + y)

  /***** Parser laws *****/
  val altRightNeutral = "anise" <|> empty
  val altLeftNeutral = empty | "coriander"

  val altLeftBiasedChoice = pure("ginger") <|> "garlic"

  val apRightAbsorb = empty <*> optional("fennel")
  val apHomomorphism = pure((x: Int) => x + 1) <*> pure(1)
  val fmap = pure((s: String) => s + "thyme") <*> "rosemary"

  val fmapLeftAbsorb = empty.map(s => add(s, "sage"))

  val fmapHomomorphism = pure("parsley").map(s => add(s, "wasabi"))

  val fmapComposition = string("saffron").map(s => add(s, "tarragon")).map(s => add(s, "turmeric"))

  def defParser(p: Parsley[String]): Parsley[String] = p.map(s => add(s, "oregano")).map(s => add(s, "paprika"))

  /***** Resugaring parsers *****/
  val thenAsAp = pure((_: String) => (y: String) => y) <*> "cumin" <*> "dill"
  val thenAsAp2 = string("cumin").map(_ => (y: String) => y) <*> "dill"
  val thenDiscardAsAp = string("cumin").map(x => (_: String) => x) <*> "dill"
}
