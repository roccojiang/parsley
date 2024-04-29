package fix.utils

import scala.meta._

// sealed abstract class Func[A] extends Product with Serializable {
sealed abstract class Func extends Product with Serializable {
  import Func._

  def term: Term

  def simplify: Func = this match {
    // identity(x) = x
    case App(Id(_), x) => x.simplify

    // f.compose(identity) = f
    case App(App(Compose, f), Id(_)) => f.simplify
    // identity.compose(f) = f
    case App(App(Compose, Id(_)), f) => f.simplify

    case App(f, x) => App(f.simplify, x.simplify)

    case _ => this
  }
}

object Func {
  case class Abs(f: Func) extends Func {
    val term = q"${f.term}(_)"
  }

  case class App(f: Func, x: Func) extends Func {
    val term = f match {
      case Compose => q"${x.term}.compose"
      case _ => q"${f.term}(${x.term})"
    }
  }


  case class Id(tpe: Type) extends Func {
    val term = q"identity[$tpe]"
  }

  case object Compose extends Func {
    val term = q"compose"
  }

  case object Flip extends Func {
    val term = q"flip"
  }

  case class Opaque(t: Term, shouldCurry: Boolean) extends Func {
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
}