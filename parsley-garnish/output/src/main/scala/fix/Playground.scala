package fix

import parsley.Parsley._
import parsley.character._
import parsley.debug._

import parsley.syntax.lift._
import parsley.lift._

object Playground {
  // case class Add(a: Int, b: Int)
  // val p = Add.lift(string("a") as 1, string("b") as 2)
  // val q = lift2(Add(_, _), string("a") as 1, string("b") as 2)

  case class Add(a: String, b: String)
  val p = Add.lift(string("a"), string("b"))
  val q = lift2(Add(_, _), string("a"), string("b"))

  // def add(a: String)(b: String) = a + b

  // def main(args: Array[String]): Unit = {
  //   val input = "parse"

  //   val test = ((string("p").map(add)).debug("p") <*> empty).debug("test")

  //   val parsed = test.parse(input)
  //   println(parsed)


  //   val test2 = (empty.debug("empty") <*> string("p").debug("p")).debug("test2")
  //   println(test2.parse(input))
  // }
}
