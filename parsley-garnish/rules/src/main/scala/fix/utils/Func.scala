package fix.utils

import scala.meta._

// sealed abstract class Func[A] extends Product with Serializable {
sealed abstract class Func extends Product with Serializable {
  def term: Term
}

object Func {
  // TODO: this is curried so idk
  case class App(f: Func, x: Func) extends Func {
    val term = f match {
      case Compose => q"${x.term}.compose(_)"
      case _ => q"${f.term}(${x.term})"
    }
  }


  case class Id(tpe: Type) extends Func {
    val term = q"identity[$tpe]"
  }

  case object Compose extends Func {
    val term = q"compose(_)"
  }

  case object Flip extends Func {
    val term = q"flip(_)"
  }

  case class Opaque(t: Term) extends Func {
    val term = t
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