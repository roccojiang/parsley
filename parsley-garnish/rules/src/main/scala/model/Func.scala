package model

import scalafix.v1._

import scala.meta._
import Func._
import analysis.MethodParametersAnalyzer
import analysis.MethodParametersAnalyzer.{ConcreteArg, FuncArgument, ParameterArg}
import analysis.TypeSignatureAnalyzer.getInferredTypeSignature

sealed abstract class Func extends Product with Serializable {

  def term: Term

  // TODO: is variable capture an issue, can we assume Barendregt's convention? (see TSfPL notes?)
  def substitute(x: Var, y: Func): Func = this match {
    case `x` => y
    case Lam(xs, f) => Lam(xs, f.substitute(x, y))
    case App(f, xs @ _*) => App(f.substitute(x, y), xs.map(_.substitute(x, y)): _*)
    case _ => this
  }

  def simplify: Func = if (this.normal) this else this.reduce

  def reduce: Func = this match {
    // Beta reduction rule
    case App(Lam(xs, f), ys @ _*) =>
      // TODO: better error handling than this
      assert(xs.length == ys.length, "Incorrect number of arguments")
      println(s"SIMPLIFYING $this")

      xs.zip(ys).foldRight(f) { case ((x, y), acc) => acc.substitute(x, y) }.reduce

    case App(f, xs @ _*) => f.reduce match {
      case g: Lam => App(g, xs.map(_.reduce): _*).reduce
      case g => App(g, xs.map(_.reduce): _*)
    }
    case Lam(xs, f) => Lam(xs, f.reduce)

    case _ => this
  }

  private def normal: Boolean = this match {
    case App(Lam(_, _), _) => false
    case App(f, xs @ _*) => f.normal && xs.forall(_.normal)
    case _ => true
  }

  override def toString: String = term.syntax
}

object Func {
  // TODO: can t be guaranteed to be a Term.Name?
  case class Opaque(t: Term) extends Func {
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
      val params = xs.map(x => Term.Param(List.empty, x.term, x.displayType.map(t => Type.Name(t.toString)), None))
      q"(..$params) => ${f.term}"
    }
  }
  object Lam {
    /* x: T, f: R, \x.f : T => R */
    def apply(x: Var, f: Func): Func = Lam(List(x), f)
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

  private def toFunc(f: Opaque, args: List[List[FuncArgument]]): Func = {
    // alpha conversion: assign fresh variable names
    val freshArgs = args.map(_.map {
      case ParameterArg(_, tpe) => ParameterArg(Term.fresh(), tpe)
      case arg => arg
    })

    val apps = freshArgs.foldLeft(f: Func) { (acc, args) => {
      val params = args.map {
        case ParameterArg(name, tpe) => Var(name, tpe.map(_.tpe))
        case ConcreteArg(arg, _)     => Opaque(arg)
      }
      App(acc, params: _*)
    }}

    freshArgs.foldRight(apps) { (args, acc) => {
      val params = args.collect {
        case ParameterArg(name, tpe) => Var(name, tpe.map(_.tpe))
      }
      if (params.isEmpty) acc else Lam(params, acc)
    }}
  }

  def buildFuncFromTerm(f: Term, debugName: String)(implicit doc: SemanticDocument): Func = {
    def buildFunc(g: Term.Name) = {
      println(s"${g.structure} func within $debugName: ${g.synthetics} | ${g.synthetics.structure} )}") // TODO: this seems to find the correct Term.Names
      val typeSignature = getInferredTypeSignature(g)
      println(s">> typeSignature: $typeSignature")
      val funcArgs = MethodParametersAnalyzer.getFuncArguments(f)
      val funcArgsWithTypes = MethodParametersAnalyzer.updateFuncArgTypes(funcArgs, typeSignature)

      val lambdaTerm = Func.toFunc(Opaque(g), funcArgsWithTypes)
      println(s">> LAMBDATERM: $lambdaTerm")
      lambdaTerm
    }

    def tryBuildFunc(f: Term): Option[Func] = {
      println(s"$debugName: ${f.structure}; sig = ${f.symbol.info.map(_.signature.structure)}")

      f.collect {
        case Term.Apply.After_4_6_0(g: Term.Name, _) =>
          // println(s"$g symbol sig ${g.symbol.info.map(i => i.signature.structure)}") // this gets a class signature, it seems
          println(">> Term.Apply(Term.Name) case")
          buildFunc(g)
      }.headOption.orElse(f match {
        case f: Term.Name =>
          println(">> Just Term.Name case")
          Some(buildFunc(f))
        case _ => None
      })
    }

    tryBuildFunc(f).getOrElse(Opaque(f))
  }
}
