package fix.utils

import scala.meta._

sealed abstract class Func[+A] extends Product with Serializable {
  import Func._

  def term: Term

  def simplify = ???

  // def simplify: Func = this match {
  //   // case App(Abs(x, f), y) => f.substitute(x, y).simplify

  //   // identity(x) = x
  //   case App(Id(_), x) => x.simplify

  //   // f.compose(identity) = f
  //   case App(App(Compose, f), Id(_)) => f.simplify
  //   // identity.compose(f) = f
  //   case App(App(Compose, Id(_)), f) => f.simplify

  //   case App(f, x) => App(f.simplify, x.simplify)

  //   case _ => this
  // }
}

object Func {
  case class Abs[A, B](x: A, f: Func[B]) extends Func[A => B] {
    // val term = q"($x) => ${f.term}"
    val term = ???

    // val test = (a: Term.Param) => (b) => Abs(a, b)
  }

  case class App[A, B](f: Func[A => B], x: Func[A]) extends Func[B] {
    // val term = f match {
    //   case Compose => q"${x.term}.compose"
    //   case _ => q"${f.term}(${x.term})"
    // }
    val term = ???
  }


  case class Id[T <: Type](tpe: T) extends Func[T => T] {
    val term = q"identity[$tpe]"

    val t: Term = Term.fresh()
  }

  case class Compose[A, B, C]() extends Func[(B => C) => (A => B) => (A => C)] {
    val term = q"compose"
  }

  case class Flip[A, B, C]() extends Func[(A => B => C) => (B => A => C)] {
    val term = q"flip"
  }

  case class Opaque[A](t: Term, shouldCurry: Boolean) extends Func[A] {
    val term = if (shouldCurry) p"$t.curried" else t
  }

  // case class App[A, B](f: Func[A => B], x: Func[A]) extends Func[B] {
  //   val term = q"${f.term}(${x.term})"
  // }

  // case class Id[A](tpe: Type) extends Func[A => A] {
  //   val term = q"identity[$tpe]"
  // }

  // case class Compose[A, B, C]() extends Func[(B => C) => (A => B) => A => C] {
  //   val term = q"compose"
  // }

  def flip[A, B, C](f: Func[A => B => C]): Func[B => A => C] = App(Flip[A, B, C](), f)

  def compose[A, B, C](f: Func[B => C])(g: Func[A => B]): Func[A => C] = App(App(Compose[A, B, C](), f), g)


  val test = compose(Id[Type](Type.Name("Int")))
  val test2 = flip

  // implicit class FuncOps[A, B, C](f: Func[B => C]) {
  //   def compose(g: Func[A => B]): Func[A => C] = App(App(Compose[A, B, C](), f), g)
  // }
  // implicit class FuncOps2[A, B, C](f: Func[A => B => C]) {
  //   def flip(): Func[B => A => C] = App(Flip(), f)
  // }
}
