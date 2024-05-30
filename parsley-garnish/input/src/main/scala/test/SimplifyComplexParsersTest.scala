/* 
rule = SimplifyComplexParsers
 */
package test

import parsley.Parsley, parsley.Parsley._
import parsley.character._
import parsley.combinator._
import parsley.syntax.character._
import parsley.generic._

object SimplifyComplexParsersTest {
    def add[A](a: A, b: String): String = a.toString + b

    sealed trait Expr
    case class Add(x: Expr, y: Expr) extends Expr
    case class Sub(x: Expr, y: Expr) extends Expr
    case class Mul(x: Expr, y: Expr) extends Expr
    case class Neg(x: Expr) extends Expr
    case class Num(x: Int) extends Expr

    object Add extends ParserBridge2[Expr, Expr, Add]
    object Sub extends ParserBridge2[Expr, Expr, Sub]
    object Mul extends ParserBridge2[Expr, Expr, Mul]
    object Neg extends ParserBridge1[Expr, Neg]
    object Num extends ParserBridge1[Int, Num]

    val HELLO = (char('*') ~> pure((x: Expr) => (y: Expr) => Mul(x, y))) <*> negate
    lazy val negate: Parsley[Expr] = Neg(string("negate") ~> negate) | atom
    lazy val atom: Parsley[Expr] = Num(item.map(_.asDigit))

    val altRightNeutral = "anise" <|> empty
    val altLeftNeutral = empty | "coriander"

    val altLeftBiasedChoice = pure("ginger") <|> "garlic"

    val apRightAbsorb = empty <*> optional("fennel")
    val apHomomorphism = pure((x: Int) => x + 1) <*> pure(1)
    val fmap = pure((s: String) => s + "thyme") <*> "rosemary" // TODO: this doesn't compile

    val fmapLeftAbsorb = empty.map(s => add(s, "sage"))

    val fmapHomomorphism = pure("parsley").map(s => add(s, "wasabi"))

    val fmapComposition = string("saffron").map(s => add(s, "tarragon")).map(s => add(s, "turmeric"))

    def defParser(p: Parsley[String]): Parsley[String] = p.map(s => add(s, "oregano")).map(s => add(s, "paprika"))
}
