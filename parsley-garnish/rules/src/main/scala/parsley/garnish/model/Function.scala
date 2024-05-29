package parsley.garnish.model

import scala.collection.mutable
import scala.meta._
import scalafix.v1._

sealed abstract class Function extends Product with Serializable {
  import Function._

  def term: Term

  def normalise: Function = this.reflect.normalise.reify
  // {
  //   println(s"1) NORMALISING $this")
  //   val reflected = this.reflect
  //   println(s"2) REFLECTED TO ${reflected}")
  //   val normalised = reflected.normalise
  //   println(s"3) NORMALISED TO $normalised")
  //   val reified = normalised.reify
  //   println(s"4) REIFIED TO $reified")
  //   reified
  // }

  private def reflect: HOAS = {
    def reflect0(func: Function, boundVars: Map[Var, HOAS]): HOAS = func match {
      case v @ Var(name, displayType) =>
        boundVars.getOrElse(v, HOAS.Var(name, displayType))
      case Lam(xs, f) =>
        HOAS.Abs(xs.size, vs => reflect0(f, boundVars ++ xs.zip(vs)))
      case App(f, xs @ _*) =>
        HOAS.App(reflect0(f, boundVars), xs.map(reflect0(_, boundVars)))
      case Translucent(term, env) =>
        HOAS.Translucent(term, env.map { case (v, func) => v -> reflect0(func, boundVars) })
    }
   
    reflect0(this, Map.empty)
  }

  // TODO: remove this - old normalisation by substitution approach
  /*
  def normalise: Function = if (this.normal) this else this.reduce

  // Barendregt's convention is enforced
  def substitute(x: Var, y: Function): Function = this match {
    case Var(name, _) if name == x.name => y
    case Lam(xs, f) => Lam(xs, f.substitute(x, y))
    case App(f, xs @ _*) => App(f.substitute(x, y), xs.map(_.substitute(x, y)): _*)
    case Translucent(t, env) =>
      Translucent(t, env.map { case (v, f) => (v, f.substitute(x, y)) } + (x.name -> y))
    case _ => this
  }

  def reduce: Function = this match {
    // Beta reduction rule
    case App(Lam(xs, f), ys @ _*) =>
      assert(xs.length == ys.length, "Incorrect number of arguments")
      xs.zip(ys).foldRight(f) { case ((x, y), acc) => acc.substitute(x, y) }.reduce

    case App(f, xs @ _*) => f.reduce match {
      case g: Lam => App(g, xs.map(_.reduce): _*).reduce
      case g => App(g, xs.map(_.reduce): _*)
    }
    case Lam(xs, f) => Lam(xs, f.reduce)

    case Translucent(t, substs) => Translucent(t, substs.map { case (x, y) => x -> y.reduce })

    case _ => this
  }

  private def normal: Boolean = this match {
    case App(Lam(_, _), _*) => false
    case App(f, xs @ _*) => f.normal && xs.forall(_.normal)
    case Translucent(_, substs) => substs.values.forall(_.normal)
    case _ => true
  }
  */

  // override def toString: String = term.syntax
  // override def toString: String = this match {
  //   case Opaque(t, _) => s"${Console.RED}${t.syntax}${Console.RESET}"
  //   case App(f, xs @ _*) => s"(${f.toString})${Console.BLUE}(${xs.map(_.toString).mkString(", ")})${Console.RESET}"
  //   case Var(name, tpe) => name + (if (tpe.nonEmpty) s": ${tpe.get}" else "")
  //   case Lam(xs, f) => s"${Console.GREEN}\\(${xs.map(_.term.syntax).mkString(", ")}) -> ${f.toString}${Console.RESET}"
  // }
}

object Function {
  case class Translucent(originalTerm: Term, env: Map[VarName, Function] = Map.empty) extends Function {
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
  case class Var(name: VarName, displayType: Option[Type]) extends Function {
    val term = if (displayType.nonEmpty) q"$name: ${displayType.get}" else Term.Name(name)
    val unlabelledTerm = Term.Name(name)
  }

  object Var {
    def fresh(prefix: Option[String] = None, displayType: Option[Type] = None): Var = {
      val name = Term.fresh(prefix.getOrElse("_x"))
      Var(name.syntax, displayType)
    }
  }

  // case class Var(prefix: Option[String] = None, displayType: Option[Type] = None) extends Function {
  //   // Enforce Barendregt's convention
  //   val name = Term.fresh(prefix.getOrElse("_x"))
  //   val term = if (displayType.nonEmpty) q"$name: ${displayType.get}" else name
  // }
  // object Var {
  //   def unapply(v: Var): Option[(Term.Name, Option[Type])] = Some((v.name, v.displayType))
  // }

  /* xs: (T1, T2, ..., TN), f: R, \(x1, x2, ..., xn).f : (T1, T2, ..., TN) => R */
  case class Lam(xs: List[Var], f: Function) extends Function {
    val term = {
      val params = xs.map(x => Term.Param(List.empty, Term.Name(x.name), x.displayType, None))

      f match {
        // TODO: this breaks things for a few edge cases, so disabled for now
        // Syntactic sugar: transform single parameter lambdas to use placeholder syntax
        // case Opaque(t) if params.size == 1 =>
        //   t.transform {
        //     case v: Term.Name if v.isEqual(xs.head.name) => Term.Placeholder()
        // }.asInstanceOf[Term]
        
        case _ => q"(..$params) => ${f.term}"
      }
    }
  }
  object Lam {
    /* x: T, f: R, \x.f : T => R */
    def apply(x: Var, f: Function): Function = Lam(List(x), f)
  }

  /* f: (T1, T2, ..., TN) => R, xs: (T1, T2, ..., TN), f xs : R */
  case class App(f: Function, xs: Function*) extends Function {
    val term = q"${f.term}(..${xs.toList.map(_.term)})"
  }

  /* id : A => A */
  def id: Function = {
    val x = Var.fresh() // : A
    Lam(x, x)
  }

  /* flip : (A => B => C) => B => A => C */
  def flip: Function = {
    val f = Var.fresh() // : A => B => C
    val x = Var.fresh() // : B
    val y = Var.fresh() // : A

    // \f -> \x -> \y -> f y x
    Lam(f, Lam(x, Lam(y, App(App(f, y), x))))
  }

  /* compose : (B => C) => (A => B) => A => C */
  def compose: Function = {
    val f = Var.fresh() // : B => C
    val g = Var.fresh() // : A => B
    val x = Var.fresh() // : A

    // \f -> \g -> \x -> f (g x)
    Lam(f, Lam(g, Lam(x, App(f, App(g, x)))))
  }
  def composeH(f: Function /* B => C */): Function /* (A => B) => A => C) */ = App(compose, f)
  def composeH(f: Function /* B => C */ , g: Function /* A => B */): Function /* A => C */ = App(App(compose, f), g)

  def cons: Function = {
    val x = Var.fresh() // : A
    val xs = Var.fresh() // : List[A]

    // \x -> \xs -> x :: xs
    Lam(x, Lam(xs, App(App(Translucent(q"::"), x), xs))) // TODO: infix operators
  }
  def consH(x: Function, xs: Function): Function = App(App(cons, x), xs)

  private def buildFromFunctionTerm(func: Term.Function)(implicit doc: SemanticDocument): Function = {
    type ParameterLists = List[List[Term.Param]]

    @annotation.tailrec
    def recurseParamLists(t: Term, acc: ParameterLists): (ParameterLists, Term) = t match {
      case Term.Function.After_4_6_0(params, body) => recurseParamLists(body, params.values :: acc)
      case _ => (acc, t)
    }

    val (reversedParamLists, body) = recurseParamLists(func, List.empty)

    // Replace each method parameter with a fresh variable to preserve Barendregt's convention
    val freshReplacements = reversedParamLists.reverse.map(_.collect {
      case p @ Term.Param(_, _, decltpe, _) => p.symbol -> Var.fresh(Some("_l"), decltpe)
    })
    val freshParams = freshReplacements.map(_.map(_._2))
    val symbolsMap = freshReplacements.flatten.toMap

    // Substitute the fresh variables into the function body
    // Comparison using symbols negates scoping problems
    val updatedBody = body.transform {
      case t: Term.Name if symbolsMap contains t.symbol => symbolsMap(t.symbol).unlabelledTerm
    }.asInstanceOf[Term]

    val defaultMap = freshParams.flatten.map(v => v.name.toString -> v).toMap
    println(s"FRESH_PARAMS for $updatedBody: $defaultMap")

    val opaque = Translucent(updatedBody, defaultMap)
    println(s"BUILT OPAQUE: $opaque")

    freshParams.foldRight[Function](Translucent(updatedBody, defaultMap)) { (params, acc) => Lam(params, acc) }
  }

  private def buildFromAnonFunctionTerm(func: Term.AnonymousFunction): Function = {
    val namedParams = mutable.ListBuffer.empty[Var]

    // This assumes an in-order traversal, although I'm not aware if this is guaranteed
    val transformedFunc = func.transform {
      case Term.Ascribe(Term.Placeholder(), tpe) =>
        val namedParam = Var.fresh(Some("_p"), Some(tpe))
        namedParams += namedParam
        namedParam.unlabelledTerm
      case _: Term.Placeholder =>
        val namedParam = Var.fresh(Some("_p"), None)
        namedParams += namedParam
        namedParam.unlabelledTerm
    }.asInstanceOf[Term]

    val defaultMap = namedParams.map(v => v.name.toString -> v).toMap
    println(s"NAMED_PARAMS: $defaultMap")

    namedParams.toList.foldRight[Function](Translucent(transformedFunc, defaultMap)) { (param, acc) => Lam(param, acc) }
  }

  def buildFuncFromTerm(f: Term, debugName: String)(implicit doc: SemanticDocument): Function = {
    parsley.garnish.utils.printInfo(f, debugName)

    println(s"BUILDING FUNCTION FROM TERM: $f")

    val func = f match {
      // * Lambda - look at parameter lists
      case func: Term.Function => buildFromFunctionTerm(func)
      // * Lambda with placeholder syntax (similar as above)
      case func: Term.AnonymousFunction => buildFromAnonFunctionTerm(func)
      // * Otherwise - just treat as opaque
      // This should be fine? We'll just do function application like func(arg1)(arg2) without being able to substitute
      case _ => Translucent(f)
    }

    println(s"\t RESULTFUNC: $func")
    func
  }
}
