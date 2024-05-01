package fix.utils

import scala.meta._

sealed abstract class Func[+A] extends Product with Serializable {

  def term: Term

  def substitute[B](a: Func.Var[B], b: Func[B]): Func[A]
  // def substitute(a: Func.Var[_], b: Func.Var[_]): Func[A] = this match {
    // case Func.Lam(x, f) => Func.Lam(x, f.substitute(a, b))
  // }

  def simplify: Func[A]

  /*
  def simplify: Func[A] = {
    // def simplifyApp[B, C](f: Func[B => C], x: Func[B], isMethod: Boolean): Func[C] = (f, x) match {
    //   case (Id(_) /*: Func[C => C] */, x) => {
    //     x.asInstanceOf[Func[C]].simplify
    //   }

    //   // case (App(c: Func.Compose[A, B, C], g: Func[C], _), id: Func.Id[A]) => g.simplify

    //   case (f, x) => App(f.simplify, x.simplify, isMethod)
    // }

    // def simplifyAppApp[B, C, D](f: Func[B => C => D], x: Func[B], y: Func[C], isMethod: Boolean): Func[D] = (f, x, y) match {
    //   case (Compose(), f, g) => {
        
    //   }
    // }

    this match {
      // case App(Abs(x, f), y) => f.substitute(x, y).simplify

      // identity(x) = x
      // case App(Id(_), x, _) => x.simplify

      // f.compose(identity) = f
      // case Compose[A, B, C](f, Id(_)) => f.simplify
      // case App(App(Compose(), f, _), Id(_), _) => f.simplify
      // identity.compose(f) = f
      // case App(App(Compose, Id(_)), f) => f.simplify

      // case App(f, x, isMethod) => App(f.simplify, x.simplify, isMethod)

      // case App(f, x, isMethod) => simplifyApp(f, x, isMethod)

      case _ => this
    }
  }
  */
}

object Func {
  case class Opaque[A](t: Term) extends Func[A] {
    val term = t

    override def substitute[B](x: Var[B], y: Func[B]): Func[A] = this

    override def simplify: Opaque[A] = this
  }

  // TODO: make bound variable generation lazy somehow?
  case class Var[A](name: Term.Name) extends Func[A] {
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

  def compose[A, B, C]: Func[(B => C) => (A => B) => A => C] = {
    val f = Var[B => C](Term.fresh())
    val g = Var[A => B](Term.fresh())
    val x = Var[A](Term.fresh())

    Lam(f, Lam(g, Lam(x, App(f, App(g, x)))))
  }

  // implicit class FuncOps2[A, B, C](f: Func[(B => C) => (A => C)]) {
  //   def app(x: Func[B => C]): Func[A => C] = f match {
  //     case Abs(y: Func[B => C] @unchecked, g) => g match {
  //       case Compose(`y`, h: Func[A => B] @unchecked) => Compose(x, h)
  //       case Compose(h, `y`) => Compose(h, x)
  //     }
  //   }
  // }

}


object Test2 {
  sealed trait Exp[T]
case class Num(n: Int) extends Exp[Int]
case class Plus(e1: Exp[Int], e2: Exp[Int]) extends Exp[Int]
case class Var[T](name: String) extends Exp[T]
case class Lambda[T, U](x: Var[T], e: Exp[U]) extends Exp[T => U]
case class App[T, U](f: Exp[T => U], e: Exp[T]) extends Exp[U]

abstract class Env { outer =>
  def apply[T](x: Var[T]): T

  def + [T](xe: (Var[T], T)) = new Env {
    def apply[T](x: Var[T]): T =
      if (x == xe._1) xe._2.asInstanceOf[T]
      else outer(x)
  }
}

object Env {
  val empty = new Env {
    def apply[T](x: Var[T]): T = ???
  }
}

object Test {

  val exp = App(Lambda(Var[Int]("x"), Plus(Var[Int]("x"), Num(1))), Var[Int]("2"))

  def eval[T](e: Exp[T])(env: Env): T = e match {
    case Num(n) => n
    case Plus(e1, e2) => eval(e1)(env) + eval(e2)(env)
    case v: Var[T] => env(v)
    case Lambda(x: Var[s], e) => ((y: s) => eval(e)(env + (x -> y)))
    case App(Lambda(x, e), v) => ???
    case App(App(Lambda(x, Lambda(y, e)), v), w) => ???
    case App(f, e) => eval(f)(env)(eval(e)(env))
  }

  eval(exp)(Env.empty)
}
}