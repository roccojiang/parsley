package parsley.garnish

import scala.collection.mutable
import scala.meta._
import scala.meta.contrib._
import scalafix.v1._

object implicits {
  implicit class TreeOps(private val tree: Tree) extends AnyVal {
    def containsAnyOf(terms: Term*): Boolean = tree.collect {
      case t: Term if terms.toSeq.exists(term => t.isEqual(term)) => t
    }.nonEmpty

    def isWithinScope(scope: Tree): Boolean =
      tree.isEqual(scope) || tree.parent.exists(_.isWithinScope(scope))
  }

  implicit class TermOps(private val term: Term) extends AnyVal {
    import model.{Function, Parser}, Function._, Parser._

    def toFunction(debugName: String)(implicit doc: SemanticDocument): Function = {
      parsley.garnish.utils.printInfo(term, debugName)

      println(s"BUILDING FUNCTION FROM TERM: $term")

      val func = term match {
        case f: Term.Function => buildFromFunctionTerm(f)
        case f: Term.AnonymousFunction => buildFromAnonFunctionTerm(f)
        case _ => Translucent(term)
      }

      println(s"\t RESULTFUNC: $func")
      func
    }

    def toParser(implicit doc: SemanticDocument): Parser = {
      val transforms: PartialFunction[Term, Parser] = Seq(
        Pure.fromTerm,
        Empty.fromTerm,
        Choice.fromTerm,
        Ap.fromTerm,
        FMap.fromTerm,
        Many.fromTerm,
        SomeP.fromTerm,
        Str.fromTerm,
        LiftImplicit.fromTerm,
        LiftExplicit.fromTerm,
        Zipped.fromTerm,
        Bridge.fromTerm,
      ).reduce(_ orElse _)

      if (transforms.isDefinedAt(term)) transforms(term)
      else term match {
        // See https://scalacenter.github.io/scalafix/docs/developers/symbol-matcher.html#unapplytree for how to mitigate
        // against matching multiple times using SymbolMatchers

        // any other unrecognised term names will be assumed to be a non-terminal
        // this is a conservative approach, it might assume some Parsley combinators are actually NTs?
        case t: Term.Name => NonTerminal(t.symbol)

        case unrecognisedTerm => Unknown(unrecognisedTerm)
      }
    }

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
        case t: Term.Name if symbolsMap contains t.symbol => symbolsMap(t.symbol).unannotatedTerm
      }.asInstanceOf[Term]

      val defaultMap = freshParams.flatten.map(v => v.name.toString -> v).toMap

      freshParams.foldRight[Function](Translucent(updatedBody, defaultMap)) { (params, acc) => Lam(params, acc) }
    }

    private def buildFromAnonFunctionTerm(func: Term.AnonymousFunction): Function = {
      val namedParams = mutable.ListBuffer.empty[Var]

      // This assumes an in-order traversal, although I'm not aware if this is guaranteed
      val transformedFunc = func.transform {
        case Term.Ascribe(Term.Placeholder(), tpe) =>
          val namedParam = Var.fresh(Some("_p"), Some(tpe))
          namedParams += namedParam
          namedParam.unannotatedTerm
        case _: Term.Placeholder =>
          val namedParam = Var.fresh(Some("_p"), None)
          namedParams += namedParam
          namedParam.unannotatedTerm
      }.asInstanceOf[Term]

      val params = namedParams.toList
      val defaultMap = params.map(v => v.name.toString -> v).toMap

      params.foldRight[Function](Translucent(transformedFunc, defaultMap)) { (param, acc) => Lam(param, acc) }
    }
  }
}
