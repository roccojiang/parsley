package parsley.garnish.model

import scala.meta._

sealed abstract class Expr extends Product with Serializable {
  import Expr._

  def term: Term

  def isEquivalent(other: Expr): Boolean = this.evaluate.reify == other.evaluate.reify

  def normalise: Expr = this.evaluate.reify

  private def evaluate: Sem = {
    def eval(func: Expr, boundVars: Map[Var, Sem]): Sem = func match {
      case v @ Var(name, displayType) =>
        boundVars.getOrElse(v, Sem.Var(name, displayType))
      case AbsN(xs, f) =>
        Sem.Abs(xs.map(_.displayType), vs => eval(f, boundVars ++ xs.zip(vs)))
      case App(f, xs) => eval(f, boundVars) match {
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
  sealed trait Lambda extends Expr

  case class Translucent(originalTerm: Term, env: Map[VarName, Expr] = Map.empty) extends Lambda {
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

  type VarName = String
  case class Var(name: VarName, displayType: Option[Type]) extends Lambda {
    val term = Term.Name(name)
  }

  object Var {
    def fresh(prefix: Option[String] = None, displayType: Option[Type] = None): Var = {
      val name = Term.fresh(prefix.getOrElse("_x"))
      Var(name.syntax, displayType)
    }
  }

  /* xs: (T1, T2, ..., TN), f: R, \(x1, x2, ..., xn).f : (T1, T2, ..., TN) => R */
  case class AbsN(xs: List[Var], f: Expr) extends Lambda {
    val term = {
      // Vars are only annotated with their types in this position
      val params = xs.map(x => Term.Param(List.empty, Term.Name(x.name), x.displayType, None))
      q"(..$params) => ${f.term}" // TODO: possible to introduce placeholder syntax / eta reduce?
    }
  }
  object Abs {
    /* x: T, f: R, \x.f : T => R */
    def apply(x: Var, f: Expr): Expr = AbsN(List(x), f)

    def unapply(abs: AbsN): Option[(Var, Expr)] = abs match {
      case AbsN(List(x), f) => Some((x, f))
      case _ => None
    }
  }

  /* f: (T1, T2, ..., TN) => R, xs: (T1, T2, ..., TN), f xs : R */
  case class App(f: Expr, xs: List[Expr]) extends Lambda {
    val term = q"${f.term}(..${xs.map(_.term)})"
  }
  object App {
    def apply(f: Expr, x: Expr): App = App(f, List(x))
  }

  /* id : A => A */
  def id: Expr = {
    val x = Var.fresh() // : A
    Abs(x, x)
  }

  /* flip : (A => B => C) => B => A => C */
  def flip: Expr = {
    val f = Var.fresh() // : A => B => C
    val x = Var.fresh() // : B
    val y = Var.fresh() // : A

    // \f -> \x -> \y -> f y x
    Abs(f, Abs(x, Abs(y, App(App(f, y), x))))
  }

  /* compose : (B => C) => (A => B) => A => C */
  def compose: Expr = {
    val f = Var.fresh() // : B => C
    val g = Var.fresh() // : A => B
    val x = Var.fresh() // : A

    // \f -> \g -> \x -> f (g x)
    Abs(f, Abs(g, Abs(x, App(f, App(g, x)))))
  }
  def composeH(f: Expr /* B => C */): Expr /* (A => B) => A => C) */ = App(compose, f)
  def composeH(f: Expr /* B => C */ , g: Expr /* A => B */): Expr /* A => C */ = App(App(compose, f), g)

  def cons: Expr = {
    val x = Var.fresh() // : A
    val xs = Var.fresh() // : List[A]

    // \x -> \xs -> x :: xs
    Abs(x, Abs(xs, App(App(Translucent(q"::"), x), xs))) // TODO: infix operators
  }
  def consH(x: Expr, xs: Expr): Expr = App(App(cons, x), xs)
}

private sealed abstract class Sem extends Product with Serializable {
  import parsley.garnish.utils.Fresh
  import Sem._

  def reify: Expr = {
    def reify0(func: Sem)(implicit freshSupply: Fresh): Expr = func match {
      case Abs(tpes, f) =>
        val params = tpes.map(Expr.Var(freshSupply.next(), _))
        Expr.AbsN(params, reify0(f(params.map { case Expr.Var(name, tpe) => Sem.Var(name, tpe) } )))
      case App(f, xs) => Expr.App(reify0(f), xs.map(reify0))
      case Translucent(t, env) => Expr.Translucent(t, env.view.mapValues(reify0).toMap)
      case Var(name, displayType) => Expr.Var(name, displayType)
    }

    reify0(this)(new Fresh)
  }
}

private object Sem {
  case class Abs(paramTypes: List[Option[Type]], f: List[Sem] => Sem) extends Sem
  object Abs {
    def apply(f: List[Sem] => Sem): Abs = Abs(List(None), f)
  }

  case class App(f: Sem, xs: List[Sem]) extends Sem
  object App {
    def apply(f: List[Sem], xs: List[Sem]): App = {
      assert(f.size == 1)
      App(f.head, xs)
    }
  }

  case class Var(name: Expr.VarName, displayType: Option[Type]) extends Sem

  case class Translucent(t: Term, env: Map[Expr.VarName, Sem] = Map.empty) extends Sem
}
