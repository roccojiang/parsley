package fix.utils

import scala.meta._

sealed abstract class Func[+A] extends Product with Serializable {
  def term: Term

  def substitute[B](a: Func.Var[B], b: Func[B]): Func[A]
  def simplify: Func[A]
}

object Func {
  case class Opaque[A](t: Term) extends Func[A] {
    // val term = q"$t.curried" // TODO: don't necessitate having to curry
    val term = t

    override def substitute[B](x: Var[B], y: Func[B]): Func[A] = this

    override def simplify: Opaque[A] = this
  }

  // TODO: make bound variable generation lazy somehow?
  case class Var[A](name: Term.Name, displayType: Option[Type] = None) extends Func[A] {
    val term = name

    override def substitute[B](x: Var[B], y: Func[B]): Func[A] =
      if (this == x) y.asInstanceOf[Func[A]] /* A =:= B */ else this
    
    override def simplify: Var[A] = this
  }

  case class Lam[A, B](x: Var[A], f: Func[B]) extends Func[A => B] {
    val term = {
      val param = Term.Param(List.empty, x.term, None, None)
      q"($param) => ${f.term}"
    }

    override def substitute[C](x: Var[C], y: Func[C]): Func[A => B] =
      Lam(this.x, this.f.substitute(x, y))
    
    override def simplify: Func[A => B] = Lam(x.simplify, f.simplify)
  }

  case class App[A, B](f: Func[A => B], x: Func[A]) extends Func[B] {
    val term = q"${f.term}(${x.term})"

    override def substitute[C](x: Var[C], y: Func[C]): Func[B] =
      App(f.substitute(x, y), this.x.substitute(x, y))

    override def simplify: Func[B] = f match {
      case Lam(y: Var[t], g) => g.substitute(y, x.asInstanceOf[Func[t]]).simplify
      case _ => App(f.simplify, x.simplify)
    }
  }

  def id[A]: Func[A => A] = {
    val x = Var[A](Term.fresh())
    Lam(x, x)
  }

  def flip[A, B, C]: Func[(A => B => C) => B => A => C] = {
    val f = Var[A => B => C](Term.fresh())
    val x = Var[B](Term.fresh())
    val y = Var[A](Term.fresh())

    Lam(f, Lam(x, Lam(y, App(App(f, y), x))))
  }

  def compose[A, B, C]: Func[(B => C) => (A => B) => (A => C)] = {
    val f = Var[B => C](Term.fresh())
    val g = Var[A => B](Term.fresh())
    val x = Var[A](Term.fresh())

    Lam(f, Lam(g, Lam(x, App(f, App(g, x)))))
  }

  def compose[A, B, C](f: Func[B => C])(g: Func[A => B]): Func[A => C] = {
    App(App(compose[A, B, C], f), g)
  }
}
