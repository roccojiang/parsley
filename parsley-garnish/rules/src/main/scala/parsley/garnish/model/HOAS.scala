package parsley.garnish.model

import scala.meta._
import scalafix.v1._

sealed abstract class HOAS extends Product with Serializable {
  import HOAS._

  def normalise: HOAS = this match {
    case Abs(f) => Abs(x => f(x).normalise)
    case App(f, x) => f.whnf match {
      case Abs(g) => g(x).normalise
      case g      => App(g.normalise, x.map(_.normalise))
      // case g      => App(g.normalise, x.normalise.asInstanceOf[Tuple]) // TODO: ew, but this should be guaranteed
    }
    // case Tuple(xs) => Tuple(xs.map(_.normalise))
    case Opaque(t, env) => Opaque(t, env.map { case (k, v) => k -> v.normalise })
    case _ => this
  }

  private def whnf: HOAS = this match {
    case App(f, x) => f.whnf match {
      case Abs(g) => g(x).whnf
      case g      => App(g, x)
    }
    case Opaque(t, env) => Opaque(t, env.map { case (k, v) => k -> v.whnf })
    case _ => this
  }

  def reify: Term = this match {
    // case Abs(f) => Function.Lam(Seq.empty, f(Var(Term.Name("x"))).reify)
    // case App(f, x) => Function.App(f.reify, x.reify)
    // case Opaque(t, env) => Function.Opaque(t, env.map { case (k, v) => k -> v.reify })
    // case Var(name) => Function.Var(name)
    case _ => ???
  }
}

object HOAS {
  case class Abs(f: Seq[HOAS] => HOAS) extends HOAS

  case class App(f: HOAS, xs: Seq[HOAS]) extends HOAS
  object App {
    def apply(f: Seq[HOAS], xs: Seq[HOAS]): App = {
      assert(f.size == 1) // TODO: ew
      App(f.head, xs)
    }
  }

  // TODO: this is a specialisation of Opaque (for 'unknowns'), I guess, so is this required?
  case class Var(name: Term.Name) extends HOAS

  // case class Tuple(xs: Seq[HOAS]) extends HOAS

  case class Opaque(t: Term, env: Map[String, HOAS] = Map.empty) extends HOAS

  // def appC(f: HOAS, xs: HOAS*): HOAS = xs.foldLeft(f)(App(_, _))
  def appC(f: HOAS, xs: HOAS*): HOAS = xs.foldLeft(f)((acc, x) => App(acc, Seq(x)))

  /* flip : (A => B => C) => B => A => C */
  def flip: HOAS = {
    // \f -> \x -> \y -> f y x
    // Abs(f => Abs(x => Abs(y => appC(f, y, x))))
    Abs(f => Abs(x => Abs(y => App(App(f, y), x))))
  }
}
