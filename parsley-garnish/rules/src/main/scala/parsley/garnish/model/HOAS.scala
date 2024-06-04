package parsley.garnish.model

import scala.meta._

import Function.VarName
import parsley.garnish.utils.Fresh

sealed abstract class HOAS extends Product with Serializable {
  import HOAS._

  def eval: HOAS = {
    // println(s"NORMALISING ${this.reify}")
    val evaluated = this match {
      case Abs(n, tpes, f) => Abs(n, tpes, x => f(x).eval)
      case App(f, xs) => f.whnf match {
        case Abs(n, _, g) =>
          require(n == xs.size)
          g(xs).eval
        case g         => App(g.eval, xs.map(_.eval))
      }
      case Translucent(t, env) => Translucent(t, env.map { case (k, v) => k -> v.eval })
      case _ => this
    }
    // println(s"\tNORMALISED ${this.reify} to ${normalised.reify}")
    evaluated
  }

  private def whnf: HOAS = this match {
    case App(f, xs) => f.whnf match {
      case Abs(n, _, g) =>
        require(n == xs.size)
        g(xs).whnf
      case g         => App(g, xs)
    }
    case Translucent(t, env) => Translucent(t, env.map { case (k, v) => k -> v.whnf })
    case _ => this
  }

  def reify: Function = {
    def reify0(func: HOAS)(implicit freshSupply: Fresh): Function = func match {
      case Abs(n, tpes, f) =>
        // val params = (1 to n).map(_ => Function.Var.fresh(Some("hoas"))).toList
        val params = tpes.map(Function.Var(freshSupply.next(), _))
        Function.Abs(params, reify0(f(params.map { case Function.Var(name, tpe) => HOAS.Var(name, tpe) } )))
      case App(f, xs) =>
        Function.App(reify0(f), xs.map(reify0))
      case Translucent(t, env) => Function.Translucent(t, env.map { case (v, f) => v -> reify0(f) })
      case Var(name, displayType) => Function.Var(name, displayType)
    }

    reify0(this)(new Fresh)
  }
}

object HOAS {
  case class Abs(n: Int, paramTypes: List[Option[Type]], f: List[HOAS] => HOAS) extends HOAS
  object Abs {
    def apply(f: List[HOAS] => HOAS): Abs = Abs(1, List(None), f)
  }

  case class App(f: HOAS, xs: List[HOAS]) extends HOAS
  object App {
    def apply(f: List[HOAS], xs: List[HOAS]): App = {
      assert(f.size == 1) // TODO: ew
      App(f.head, xs)
    }
  }

  case class Var(name: VarName, displayType: Option[Type]) extends HOAS

  case class Translucent(t: Term, env: Map[VarName, HOAS] = Map.empty) extends HOAS

  // def appC(f: HOAS, xs: HOAS*): HOAS = xs.foldLeft(f)(App(_, _))
  def appC(f: HOAS, xs: HOAS*): HOAS = xs.foldLeft(f)((acc, x) => App(acc, List(x)))

  /* flip : (A => B => C) => B => A => C */
  def flip: HOAS = {
    // \f -> \x -> \y -> f y x
    // Abs(f => Abs(x => Abs(y => appC(f, y, x))))
    Abs(f => Abs(x => Abs(y => App(App(f, y), x))))
  }
}
