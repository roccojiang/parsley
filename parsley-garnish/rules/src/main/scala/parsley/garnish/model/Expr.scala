package parsley.garnish.model

import scala.meta._

sealed abstract class Expr extends Product with Serializable {
  import Expr._

  def term: Term

  def isEquivalent(other: Expr): Boolean = this.evaluate.reify == other.evaluate.reify

  def curried: Expr = this match {
    case Abs(x, f) => Abs(x, f.curried)
    case AbsN(x :: xs, f) => Abs(x, AbsN(xs, f).curried)
    case _ => this
  }

  def normalise: Expr = this.evaluate.reify

  private def evaluate: Sem = {
    def eval(func: Expr, boundVars: Map[Var, Sem]): Sem = func match {
      case v @ Var(name, displayType) =>
        boundVars.getOrElse(v, Sem.Var(name, displayType))
      case AbsN(xs, f) =>
        Sem.Abs(xs.map(_.displayType), vs => eval(f, boundVars ++ xs.zip(vs)))
      case AppN(f, xs) => eval(f, boundVars) match {
        case Sem.Abs(_, g) => g(xs.map(eval(_, boundVars)))
        case g => Sem.App(g, xs.map(eval(_, boundVars)))
      }
      case Translucent(term, env) =>
        Sem.Translucent(term, env.view.mapValues(eval(_, boundVars)).toMap)
    }

    eval(this, Map.empty)
  }

  // override def toString: String = term.syntax
}

object Expr {
  type VarName = String

  final case class Translucent(originalTerm: Term, env: Map[VarName, Expr] = Map.empty) extends Expr {
    private val transformer = new Transformer {
      override def apply(tree: Tree): Tree = tree match {
        case name: Term.Name =>
          // Must compare structurally, still cannot use referential equality in this case
          // Tree.transform might be performing a copy of the parameter terms, so they aren't the same object any more
          env.get(name.value) match {
            case Some(y) => y.term
            case None => name
          }

        case node => super.apply(node)
      }
    }

    val term = transformer(originalTerm).asInstanceOf[Term]
  }

  final case class Var(name: VarName, displayType: Option[Type]) extends Expr {
    val term = Term.Name(name)
  }

  object Var {
    def fresh(prefix: Option[String] = None, displayType: Option[Type] = None): Var = {
      val name = Term.fresh(prefix.getOrElse("_x"))
      Var(name.syntax, displayType)
    }
  }

  /* xs: (T1, T2, ..., TN), f: R, \(x1, x2, ..., xn).f : (T1, T2, ..., TN) => R */
  final case class AbsN(xs: List[Var], f: Expr) extends Expr {
    val term = {
      // Vars are only annotated with their types in this position
      val params = xs.map(x => Term.Param(List.empty, Term.Name(x.name), x.displayType, None))
      q"(..$params) => ${f.term}" // TODO: possible to introduce placeholder syntax / eta reduce?
    }
  }
  object Abs {
    /* x: T, f: R, \x.f : T => R */
    def apply(x: Var, f: Expr): Expr = AbsN(List(x), f)

    def unapply(func: AbsN): Option[(Var, Expr)] = func match {
      case AbsN(List(x), f) => Some((x, f))
      case _ => None
    }
  }

  /* f: (T1, T2, ..., TN) => R, xs: (T1, T2, ..., TN), f xs : R */
  final case class AppN(f: Expr, xs: List[Expr]) extends Expr {
    val term = q"${f.term}(..${xs.map(_.term)})"
  }
  object App {
    def apply(f: Expr, x: Expr): AppN = AppN(f, List(x))
    def apply(f: Expr, xs: Expr*): Expr = xs.foldLeft(f)(App(_, _))
  }

  /* id : A => A */
  def id: Expr = {
    // x: A
    val x = Var.fresh()

    Abs(x, x)
  }

  /* flip : (A => B => C) => B => A => C */
  def flip: Expr = {
    // f: A => B => C, x: B, y: A
    val (f, x, y) = (Var.fresh(), Var.fresh(), Var.fresh())

    // \f -> \x -> \y -> f y x
    Abs(f, Abs(x, Abs(y, App(f, y, x))))
  }

  /* compose : (B => C) => (A => B) => A => C */
  def compose: Expr = {
    // f: B => C, g: A => B, x: A
    val (f, g, x) = (Var.fresh(), Var.fresh(), Var.fresh())

    // \f -> \g -> \x -> f (g x)
    Abs(f, Abs(g, Abs(x, App(f, App(g, x)))))
  }
  def compose(f: Expr /* B => C */): Expr /* (A => B) => A => C */ = App(compose, f)
  def compose(f: Expr /* B => C */ , g: Expr /* A => B */): Expr /* A => C */ = App(compose, f, g)

  def cons: Expr = {
    // x: A, xs: List[A]
    val (x, xs) = (Var.fresh(), Var.fresh())

    // \x -> \xs -> x :: xs
    Abs(x, Abs(xs, App(App(Translucent(q"::"), x), xs))) // TODO: infix operators
  }
  def cons(x: Expr, xs: Expr): Expr = App(cons, x, xs)
}

private sealed abstract class Sem extends Product with Serializable {
  import parsley.garnish.utils.Fresh
  import Sem._

  def reify: Expr = {
    def reify0(func: Sem)(implicit freshSupply: Fresh): Expr = func match {
      case Abs(tpes, f) =>
        val params = tpes.map(Expr.Var(freshSupply.next(), _))
        Expr.AbsN(params, reify0(f(params.map { case Expr.Var(name, tpe) => Sem.Var(name, tpe) } )))
      case App(f, xs) => Expr.AppN(reify0(f), xs.map(reify0))
      case Translucent(t, env) => Expr.Translucent(t, env.view.mapValues(reify0).toMap)
      case Var(name, displayType) => Expr.Var(name, displayType)
    }

    reify0(this)(new Fresh)
  }
}

private object Sem {
  final case class Abs(paramTypes: List[Option[Type]], f: List[Sem] => Sem) extends Sem
  final case class App(f: Sem, xs: List[Sem]) extends Sem
  final case class Var(name: Expr.VarName, displayType: Option[Type]) extends Sem
  final case class Translucent(t: Term, env: Map[Expr.VarName, Sem] = Map.empty) extends Sem
}
