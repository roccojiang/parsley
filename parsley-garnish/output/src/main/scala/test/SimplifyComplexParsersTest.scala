package test

import parsley.Parsley, parsley.Parsley._
import parsley.character._
import parsley.combinator._
import parsley.syntax.character._

object SimplifyComplexParsersTest {
    def add[A](a: A, b: String): String = a + b

    val altRightNeutral = "anise"
    val altLeftNeutral = "coriander"

    val altLeftBiasedChoice = pure("ginger")

    val apRightAbsorb = empty
    val apHomomorphism = pure(((x: Int) => x + 1)(1))
    val fmap = "rosemary".map((s: String) => s + "thyme") // TODO: this doesn't compile

    val fmapLeftAbsorb = empty

    val fmapHomomorphism = pure(add("parsley")("wasabi"))

    val fmapComposition = string("saffron").map(fresh9 => add(add(fresh9)("tarragon"))("turmeric"))

    def defParser(p: Parsley[String]): Parsley[String] = p.map(fresh12 => add(add(fresh12)("oregano"))("paprika"))
}
