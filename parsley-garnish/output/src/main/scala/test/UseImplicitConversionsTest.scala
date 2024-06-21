package test

import parsley.Parsley
import parsley.character.{char, digit, string}
import parsley.expr.chain
import parsley.syntax.character._

object UseImplicitConversionsTest {

  def add(x: Int, y: Int): Int = x + y
  def sub(x: Int, y: Int): Int = x - y
  def mul(x: Int, y: Int): Int = x * y
  def intDiv(x: Int, y: Int): Int = x / y

  def between[A](left: String, p: =>Parsley[A], right: String): Parsley[A] = left ~> p <~ right

  val number: Parsley[Int] = digit.foldLeft1(0)((n, d) => n * 10 + d.asDigit)

  lazy val expr: Parsley[Int] = chain.left1(term)('+'.as(add) | '-'.as(sub))
  lazy val term: Parsley[Int] = chain.left1(atom)('*'.as(mul) | "//".as(intDiv))
  lazy val atom: Parsley[Int] = between("(", expr, ")") | number

  def main(args: Array[String]): Unit = {
    println(expr.parse("1+2*3//4")) // 1 + (2 * 3) // 4 = 2
  }
}
