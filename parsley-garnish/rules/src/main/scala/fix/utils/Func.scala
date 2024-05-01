package fix.utils

import scala.meta._

sealed abstract class Func[+A] extends Product with Serializable {
  import Func._

  def term: Term

  def simplify: Func[A] = this


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
  // TODO: make bound variable generation lazy somehow?
  case class Var[A](name: Term.Name) extends Func[A] {
    val term = name
  }

  case class Lam[A, B](x: Var[A], f: Func[B]) extends Func[A => B] {
    val term = {
      val param = Term.Param(List.empty, x.term, None, None)
      q"($param) => ${f.term}"
    }

    override def simplify: Func[A] = this match {
      case Lam(f, Lam(x, Lam(y, App(App(f, y), x)))) => {
        ???
      }
      case Lam(x, f) => ???
  }
  }

  case class App[A, B](f: Func[A => B], x: Func[A], isMethod: Boolean = false) extends Func[B] {
    val term = if (isMethod) {
      val fTerm = Term.Name(f.term.toString)
      q"${x.term}.${fTerm}"

     } else q"${f.term}(${x.term})"

    //  override def simplify: Func[B] = {
    //     (f, x) match {
    //       case (App(comp: Comp[_, _, _], g, _), id: Id[_]) => g.asInstanceOf[Func[B]].simplify

    //       case _ => this
    //     }
    //  }
  }


  // Old style of defining compose
  // case class Comp[A, B, C]() extends Func[(B => C) => (A => B) => (A => C)] {
  //   val term = ???
  // }


  case class Id[T <: Type](tpe: T) extends Func[T => T] {
    val term = q"identity[$tpe]"
  }

  case class Compose[A, B, C](f: Func[B => C], g: Func[A => B]) extends Func[A => C] {
    val term = q"${f.term}.compose(${g.term})"

    override def simplify: Func[A => C] = (f, g) match {
      case (f, Id(_) /*: Func[A => A] */) => f.asInstanceOf[Func[A => C]].simplify

      case (Id(_) /*: Func[C => C] */, g) => g.asInstanceOf[Func[A => C]].simplify

      case _ => this
    }
  }

  case class Flip[A, B, C](f: Func[A => B => C]) extends Func[B => A => C] {
    val term = q"flip(${f.term})"
  }

  case class Opaque[A](t: Term, shouldCurry: Boolean = false) extends Func[A] {
    val term = if (shouldCurry) p"$t.curried" else t
  }

  implicit class FuncOps[A, B](f: Func[A => B]) {
    def app(x: Func[A]): Func[B] = f match {
      // case Abs(x, g) => g match {
      //   case Param(name) => 
      //   case Abs(x, f) =>
      //   case App(f, x, isMethod) =>
      //   case Id(tpe) =>
      //   case Compose(f, g) =>
      //   case Flip(f) =>
      //   case Opaque(t, shouldCurry) =>
      // }

      // case Param(name) => 
      // case App(f, x, isMethod) =>
      // case Id(tpe) =>
      // case Compose(f, g) => 
      // case Flip(f) =>
      // case Opaque(t, shouldCurry) =>

      case _ => ???
    }
  }

  // implicit class FuncOps2[A, B, C](f: Func[(B => C) => (A => C)]) {
  //   def app(x: Func[B => C]): Func[A => C] = f match {
  //     case Abs(y: Func[B => C] @unchecked, g) => g match {
  //       case Compose(`y`, h: Func[A => B] @unchecked) => Compose(x, h)
  //       case Compose(h, `y`) => Compose(h, x)
  //     }
  //   }
  // }

  // def flip[A, B, C](f: Func[A => B => C]): Func[B => A => C] = App(Flip[A, B, C](), f)

  // def compose[A, B, C](f: Func[B => C])(g: Func[A => B]): Func[A => C] = App(App(Compose[A, B, C](), f), g)


  // val test = compose(Id[Type](Type.Name("Int")))
  // val test2 = flip

  // implicit class FuncOps[A, B, C](f: Func[B => C]) {
  //   def compose(g: Func[A => B]): Func[A => C] = App(App(Compose[A, B, C](), f), g)
  // }
  // implicit class FuncOps2[A, B, C](f: Func[A => B => C]) {
  //   def flip(): Func[B => A => C] = App(Flip(), f)
  // }
}

// object Test {
//   trait Expr[A]
//   case class Var(name: String) extends Expr[String]
//   case class Apply[A, B](fun: Expr[B => A], arg: Expr[B]) extends Expr[A]

//   def mapSubexpressions[A, B](e: Expr[A])(f: Expr[B] => Expr[B]): Expr[A] = {
//     e match {
//       case Apply(fun, arg) => Apply(f(fun), f(arg))
//       case Var(n) => Var(n)
//     }
//   }
// }

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