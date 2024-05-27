package parsley.garnish.model

import scala.meta._

import Function.VarName

sealed abstract class HOAS extends Product with Serializable {
  import HOAS._

  def normalise: HOAS = {
    println(s"NORMALISING ${this.reify}")
    val normalised = this match {
      case Abs(_, f) => Abs(x => f(x).normalise)
      case App(f, x) => f.whnf match {
        case Abs(_, g) => g(x).normalise
        case g      => App(g.normalise, x.map(_.normalise))
        // case g      => App(g.normalise, x.normalise.asInstanceOf[Tuple]) // TODO: ew, but this should be guaranteed
      }
      // case Tuple(xs) => Tuple(xs.map(_.normalise))
      case Opaque(t, env) => Opaque(t, env.map { case (k, v) => k -> v.normalise })
      case _ => this
    }
    println(s"\tNORMALISED ${this.reify} to ${normalised.reify}")
    normalised
  }

  private def whnf: HOAS = this match {
    case App(f, x) => f.whnf match {
      case Abs(_, g) => g(x).whnf
      case g      => App(g, x)
    }
    case Opaque(t, env) => Opaque(t, env.map { case (k, v) => k -> v.whnf })
    case _ => this
  }

  def reify: Function = {
    val reified = this match {
      case Abs(n, f) => {
        val params = (1 to n).map(_ => Function.Var(Some("hoas"))).toList
        Function.Lam(params, f(params.map(x => HOAS.Opaque(x.term))).reify)
      }
      case App(f, xs) => {
        println(s"REIFYING APP $f $xs")
        Function.App(f.reify, xs.map(_.reify): _*)
      }
      case Opaque(t, env) => Function.Opaque(t, env.map { case (v, f) => v -> f.reify })
    }
    // println(s"REIFIED $this to $reified")
    reified
  }
}

object HOAS {
  case class Abs(n: Int, f: Seq[HOAS] => HOAS) extends HOAS
  object Abs {
    def apply(f: Seq[HOAS] => HOAS): Abs = Abs(1, f)
  }

  case class App private (f: HOAS, xs: Seq[HOAS]) extends HOAS
  object App {
    def apply(f: Seq[HOAS], xs: Seq[HOAS]): App = {
      assert(f.size == 1) // TODO: ew
      App(f.head, xs)
    }
  }

  // TODO: this is a specialisation of Opaque (for 'unknowns'), I guess, so is this required?
  // case class Var(name: Term.Name) extends HOAS

  // case class Tuple(xs: Seq[HOAS]) extends HOAS

  case class Opaque(t: Term, env: Map[VarName, HOAS] = Map.empty) extends HOAS

  // def appC(f: HOAS, xs: HOAS*): HOAS = xs.foldLeft(f)(App(_, _))
  def appC(f: HOAS, xs: HOAS*): HOAS = xs.foldLeft(f)((acc, x) => App(acc, Seq(x)))

  /* flip : (A => B => C) => B => A => C */
  def flip: HOAS = {
    // \f -> \x -> \y -> f y x
    // Abs(f => Abs(x => Abs(y => appC(f, y, x))))
    Abs(f => Abs(x => Abs(y => App(App(f, y), x))))
  }
}
