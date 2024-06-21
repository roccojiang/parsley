/* 
rule = UseImplicitConversions
 */
package test

import parsley.Parsley
import parsley.character.{char, digit, string}
import parsley.expr.chain

object UseImplicitConversionsTest {

  def add(x: Int, y: Int): Int = x + y
  def sub(x: Int, y: Int): Int = x - y
  def mul(x: Int, y: Int): Int = x * y
  def intDiv(x: Int, y: Int): Int = x / y

  def between[A](left: String, p: =>Parsley[A], right: String): Parsley[A] = string(left) ~> p <~ string(right)

  val number: Parsley[Int] = digit.foldLeft1(0)((n, d) => n * 10 + d.asDigit)

  lazy val expr: Parsley[Int] = chain.left1(term)(char('+').as(add) | char('-').as(sub))
  lazy val term: Parsley[Int] = chain.left1(atom)(char('*').as(mul) | string("//").as(intDiv))
  lazy val atom: Parsley[Int] = between("(", expr, ")") | number

  def main(args: Array[String]): Unit = {
    println(expr.parse("1+2*3//4")) // 1 + (2 * 3) // 4 = 2
  }
}
