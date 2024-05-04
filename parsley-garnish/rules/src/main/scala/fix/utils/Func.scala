package fix.utils

import scala.meta._
import scalafix.v1._

sealed abstract class Func extends Product with Serializable {
  import Func._

  def term: Term

  // TODO: is variable capture an issue, can we assume Barendregt's convention? (see TSfPL notes?)
  def substitute(x: Var, y: Func): Func = this match {
    case `x` => y
    case Lam(xs, f) => Lam(xs, f.substitute(x, y))
    case App(f, xs @ _*) => App(f.substitute(x, y), xs.map(_.substitute(x, y)): _*)
    case _ => this
  }

  def simplify: Func = this match {
    // Beta reduction rule
    case App(Lam(xs, f), ys @ _*) => {
      // TODO: better error handling than this
      assert(xs.length == ys.length, "Incorrect number of arguments")

      xs.zip(ys).foldRight(f) { case ((x, y), acc) => acc.substitute(x, y) }.simplify
    }

    case App(f, xs @ _*) => App(f.simplify, xs.map(_.simplify): _*)
    case Lam(xs, f) => Lam(xs, f.simplify)

    case _ => this
  }
}

object Func {
  // TODO: can t be guaranteed to be a Term.Name?
  case class UserDefined(t: Term) extends Func {
    val term = t
  }

  // TODO: make bound variable generation lazy somehow?
  case class Var(name: Term.Name = Term.fresh(), displayType: Option[SemanticType] = None) extends Func {
    val term = name
  }
  object Var {
    def apply(displayType: SemanticType): Var = Var(Term.fresh(), Some(displayType))
  }

  /* xs: (T1, T2, ..., TN), f: R, \(x1, x2, ..., xn).f : (T1, T2, ..., TN) => R */
  case class Lam(xs: List[Var], f: Func) extends Func {
    val term = {
      val params = xs.map(x => Term.Param(List.empty, x.term, None, None))
      q"(..$params) => ${f.term}"
    }
  }
  object Lam {
    /* x: T, f: R, \x.f : T => R */
    def apply[A, B](x: Var, f: Func): Func = Lam(List(x), f)
  }

  /* f: (T1, T2, ..., TN) => R, xs: (T1, T2, ..., TN), f xs : R */
  case class App(f: Func, xs: Func*) extends Func {
    val term = q"${f.term}(..${xs.toList.map(_.term)})"
  }

  /* id : A => A */
  def id: Func = {
    val x = Var() // : A
    Lam(x, x)
  }

  /* flip : (A => B => C) => B => A => C */
  def flip: Func = {
    val f = Var() // : A => B => C
    val x = Var() // : B
    val y = Var() // : A

    Lam(f, Lam(x, Lam(y, App(App(f, y), x))))
  }

  /* compose : (B => C) => (A => B) => A => C */
  def compose: Func = {
    val f = Var() // : B => C
    val g = Var() // : A => B
    val x = Var() // : A

    Lam(f, Lam(g, Lam(x, App(f, App(g, x)))))
  }

  // def compose[A, B, C](f: Func[B => C])(g: Func[A => B]): Func[A => C] = {
  //   App(App(compose[A, B, C], f), g)
  // }

  def fromTypeSignature(f: UserDefined, signature: List[List[SemanticType]]): Func = {
    val freshVars = signature.map(_.map(tpe => Var(tpe)))

    freshVars.foldRight(
      freshVars.foldLeft(f: Func) { (acc, params) => App(acc, params: _*) }
    ) { (params, acc) => Lam(params, acc) }
  }
}
