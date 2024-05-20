package parsley.garnish.model

import scala.collection.mutable
import scala.meta._
import scala.meta.contrib._
import scalafix.v1._

import parsley.garnish.analysis.MethodParametersAnalyzer
import parsley.garnish.analysis.MethodParametersAnalyzer.{ConcreteArg, FuncArgument, ParameterArg}
import parsley.garnish.analysis.TypeSignatureAnalyzer.getInferredTypeSignature

sealed abstract class Function extends Product with Serializable {
  import Function._

  def term: Term

  // Barendregt's convention is enforced
  def substitute(x: Var, y: Function): Function = this match {
    case Var(name, _) if name.isEqual(x.name) => y
    case Lam(xs, f) => Lam(xs, f.substitute(x, y))
    case App(f, xs @ _*) => App(f.substitute(x, y), xs.map(_.substitute(x, y)): _*)
    case Opaque(t) =>
      Opaque(t.transform {
        case name: Term.Name =>
          // Must compare structurally, still cannot use referential equality in this case
          // Tree.transform might be performing a copy of the parameter terms, so they aren't the same object any more
          if (name.isEqual(x.name)) y.term
          else name
      }.asInstanceOf[Term])
    case _ => this
  }

  // TODO: figure out the correct reduction/normalisation strategy
  def simplify: Function =
    if (this.normal) this else this.reduce

  def reduce: Function = {
    println(s"SIMPLIFYING: $this")
    this match {
    // Beta reduction rule
    case App(Lam(xs, f), ys @ _*) =>
      // TODO: better error handling than this
      assert(xs.length == ys.length, "Incorrect number of arguments")
      xs.zip(ys).foldRight(f) { case ((x, y), acc) => acc.substitute(x, y) }.reduce

    case App(f, xs @ _*) => f.reduce match {
      case g: Lam => App(g, xs.map(_.reduce): _*).reduce
      case g => App(g, xs.map(_.reduce): _*)
    }
    case Lam(xs, f) => Lam(xs, f.reduce)

    case _ => this
  }}

  private def normal: Boolean = this match {
    case App(Lam(_, _), _*) => false
    case App(f, xs @ _*) => f.normal && xs.forall(_.normal)
    case _ => true
  }

  // override def toString: String = term.syntax
  override def toString: String = this match {
    case Opaque(t) => s"${Console.RED}${t.syntax}${Console.RESET}"
    case App(f, xs @ _*) => s"(${f.toString})${Console.BLUE}(${xs.map(_.toString).mkString(", ")})${Console.RESET}"
    case Var(name, tpe) => name.syntax + (if (tpe.nonEmpty) s": ${tpe.get}" else "")
    case Lam(xs, f) => s"${Console.GREEN}\\(${xs.map(_.term.syntax).mkString(", ")}) -> ${f.toString}${Console.RESET}"
  }
}

object Function {
  case class Opaque(t: Term) extends Function {
    val term = t
  }

  case class Var(prefix: Option[String] = None, displayType: Option[Type] = None) extends Function {
    // Enforce Barendregt's convention
    val name = Term.fresh(prefix.getOrElse("fresh"))
    val term = if (displayType.nonEmpty) q"$name: ${displayType.get}" else name
  }
  object Var {
    def unapply(v: Var): Option[(Term.Name, Option[Type])] = Some((v.name, v.displayType))
  }

  /* xs: (T1, T2, ..., TN), f: R, \(x1, x2, ..., xn).f : (T1, T2, ..., TN) => R */
  case class Lam(xs: List[Var], f: Function) extends Function {
    val term = {
      val params = xs.map(x => Term.Param(List.empty, x.name, x.displayType, None))

      f match {
        // Syntactic sugar: transform single parameter lambdas to use placeholder syntax
        case Opaque(t) if params.size == 1 =>
          t.transform {
            case v: Term.Name if v.isEqual(xs.head.name) => Term.Placeholder()
        }.asInstanceOf[Term]
        
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
    val x = Var() // : A
    Lam(x, x)
  }

  /* flip : (A => B => C) => B => A => C */
  def flip: Function = {
    val f = Var() // : A => B => C
    val x = Var() // : B
    val y = Var() // : A

    // \f -> \x -> \y -> f y x
    Lam(f, Lam(x, Lam(y, App(App(f, y), x))))
  }

  /* compose : (B => C) => (A => B) => A => C */
  def compose: Function = {
    val f = Var() // : B => C
    val g = Var() // : A => B
    val x = Var() // : A

    // \f -> \g -> \x -> f (g x)
    Lam(f, Lam(g, Lam(x, App(f, App(g, x)))))
  }

  def composeH(f: Function /* B => C */): Function /* (A => B) => A => C) */ = App(compose, f)
  def composeH(f: Function /* B => C */ , g: Function /* A => B */): Function /* A => C */ = App(App(compose, f), g)

  /*
  private def toFunc(f: Opaque, args: List[List[FuncArgument]]): Function = {
    // alpha conversion: assign fresh variable names
    val freshArgs = args.map(_.map {
      case ParameterArg(_, tpe) => ParameterArg(Term.fresh(), tpe)
      case arg => arg
    })

    val apps = freshArgs.foldLeft(f: Function) { (acc, args) => {
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
  */

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
      case p @ Term.Param(_, _, decltpe, _) => p.symbol -> Var(Some("lambda_param"), decltpe)
    })
    val freshParams = freshReplacements.map(_.map(_._2))
    val symbolsMap = freshReplacements.flatten.toMap

    // Substitute the fresh variables into the function body
    // Comparison using symbols negates scoping problems
    val updatedBody = body.transform {
      case t: Term.Name if symbolsMap contains t.symbol => symbolsMap(t.symbol).name
    }.asInstanceOf[Term]

    freshParams.foldRight[Function](Opaque(updatedBody)) { (params, acc) => Lam(params, acc) }
  }

  private def buildFromAnonFunctionTerm(func: Term.AnonymousFunction): Function = {
    val namedParams = mutable.ListBuffer.empty[Var]

    // This assumes an in-order traversal, although I'm not aware if this is guaranteed
    val transformedFunc = func.transform {
      case Term.Ascribe(Term.Placeholder(), tpe) =>
        val namedParam = Var(Some("placeholder"), Some(tpe))
        namedParams += namedParam
        namedParam.name
      case _: Term.Placeholder =>
        val namedParam = Var(Some("placeholder"), None)
        namedParams += namedParam
        namedParam.name
    }.asInstanceOf[Term]

    namedParams.toList.foldRight[Function](Opaque(transformedFunc)) { (param, acc) => Lam(param, acc) }
  }

  def buildFuncFromTerm(f: Term, debugName: String)(implicit doc: SemanticDocument): Function = {
    parsley.garnish.utils.printInfo(f, debugName)

    val func = f match {
      // * Lambda - look at parameter lists
      case func: Term.Function => buildFromFunctionTerm(func)
      // * Lambda with placeholder syntax (similar as above)
      case func: Term.AnonymousFunction => buildFromAnonFunctionTerm(func)
      // * Otherwise - just treat as opaque
      // This should be fine? We'll just do function application like func(arg1)(arg2) without being able to substitute
      case _ => Opaque(f)
    }

    println(s"\t RESULTFUNC: $func")
    func
  }

  /*
  def buildFuncFromTerm(f: Term, debugName: String)(implicit doc: SemanticDocument): Function = {
    def buildFunc(g: Term.Name) = {
      // println(s"${g.structure} func within $debugName:
      //   ${g.synthetics} | ${g.synthetics.structure} )}")" +
      
      /*
      println(s"""${g.structure} func within $debugName:
                 |\tsynthetics = ${g.synthetics} | ${g.synthetics.structure}
                 |\tsymbol = ${g.symbol} | ${g.symbol.info.map(_.signature.structure)}
                 """.stripMargin)
                 */
      val typeSignature = getInferredTypeSignature(g)
      // println(s">> typeSignature: $typeSignature")
      val funcArgs = MethodParametersAnalyzer.getFuncArguments(f)
      val funcArgsWithTypes = MethodParametersAnalyzer.updateFuncArgTypes(funcArgs, typeSignature)

      val curriedArgs = funcArgsWithTypes.flatten.map(List(_))

      val lambdaTerm = Function.toFunc(Opaque(g), curriedArgs)
      // println(s">> LAMBDATERM: $lambdaTerm")
      lambdaTerm
    }

    def tryBuildFunc(f: Term): Option[Function] = {
      parsley.garnish.utils.printInfo(f, debugName)


      f.collect {
        case Term.Apply.After_4_6_0(g: Term.Name, _) =>
          // println(s"$g symbol sig ${g.symbol.info.map(i => i.signature.structure)}") // this gets a class signature, it seems
          buildFunc(g)
      }.headOption.orElse(f match {
        case f: Term.Name =>
          Some(buildFunc(f))
        case _ => None
      })

      // newApproach
    }

    tryBuildFunc(f).getOrElse(Opaque(f))
  }
  */
}
