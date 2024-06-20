package parsley.garnish

import scala.collection.mutable
import scala.meta._
import scala.meta.contrib._
import scalafix.v1._

import parsley.garnish.analysis.TypeSignatureAnalyzer.getInferredTypeSignature

object implicits {
  implicit class TreeOps(private val tree: Tree) extends AnyVal {
    def containsAnyOf(terms: Term*): Boolean = tree.collect {
      case t: Term if terms.toSeq.exists(term => t.isEqual(term)) => t
    }.nonEmpty

    def isWithinScope(scope: Tree): Boolean =
      tree.isEqual(scope) || tree.parent.exists(_.isWithinScope(scope))
  }

  implicit class TermOps(private val term: Term) extends AnyVal {
    import model.{Expr, Parser}, Expr._, Parser._

    def toExpr(numParams: Int = 1)(implicit doc: SemanticDocument): Expr = term match {
      case f: Term.Function          => buildFromFunctionTerm(f)
      case f: Term.AnonymousFunction => buildFromAnonFunctionTerm(f)
      case f: Term.Name =>
        // TODO: infer types, but these are SemanticTypes and we need scala.meta.Type
        val inferredTypeSig = getInferredTypeSignature(f)
        val params = if (inferredTypeSig.nonEmpty) {
          inferredTypeSig.map(_.map(_ => Var.fresh(Some("_b"), None)))
        } else {
          // backup approach: if given the number of parameters passed to the invocation of this function, create that many placeholders
          // honestly, given the current implementation, this seems better than inference since we can't get the types
          List(List.fill(numParams)(Var.fresh(Some("_b"), None)))
        }
        val body = params.foldLeft[Expr](Translucent(term))(AppN(_, _))
        params.foldRight[Expr](body)(AbsN(_, _))

      case _ => Translucent(term)
    }

    def toParser(implicit doc: SemanticDocument): Parser = {
      val transforms: PartialFunction[Term, Parser] = Seq(
        Pure.fromTerm,
        Empty.fromTerm,
        <|>.fromTerm,
        <*>.fromTerm,
        ~>.fromTerm,
        <~.fromTerm,
        FMap.fromTerm,
        ManyP.fromTerm,
        SomeP.fromTerm,
        Chr.fromTerm,
        Str.fromTerm,
        LiftImplicit.fromTerm,
        LiftExplicit.fromTerm,
        Zipped.fromTerm,
        Bridge.fromTerm,
        EndBy.fromTerm,
      ).reduce(_ orElse _)

      if (transforms.isDefinedAt(term)) transforms(term)
      else term match {
        // See https://scalacenter.github.io/scalafix/docs/developers/symbol-matcher.html#unapplytree for how to mitigate
        // against matching multiple times using SymbolMatchers

        // TODO: is there a way to flip this round to check if the owner is the current file's package? I can't find a way to get this information
        case t: Term.Name if !(t.symbol.owner.value startsWith "parsley/") => NonTerminal(t.symbol)

        case unrecognised => Unknown(unrecognised)
      }
    }

    private def buildFromFunctionTerm(func: Term.Function)(implicit doc: SemanticDocument): Expr = {
      type ParameterLists = List[List[Term.Param]]

      @annotation.tailrec
      def recurseParamLists(t: Term, acc: ParameterLists): (ParameterLists, Term) = t match {
        case Term.Function.After_4_6_0(params, body) => recurseParamLists(body, params.values :: acc)
        case _ => (acc, t)
      }

      val (reversedParamLists, body) = recurseParamLists(func, List.empty)

      // Hygiene: rename each parameter to its (unique?) symbol name
      val freshReplacements = reversedParamLists.reverse.map(_.collect {
        case p @ Term.Param(_, _, decltpe, _) =>
          // TODO: placeholder parameters will not have symbols
          // p.symbol -> Var(p.symbol.value, decltpe)
          p.symbol -> Var.fresh(Some("_l"), decltpe)
      })
      val freshParams = freshReplacements.map(_.map(_._2))
      val symbolsMap = freshReplacements.flatten.toMap

      // Substitute the fresh variables into the function body
      // Comparison using symbols negates scoping problems
      val updatedBody = body.transform {
        case t: Term.Name if symbolsMap contains t.symbol => symbolsMap(t.symbol).term
      }.asInstanceOf[Term]
      val defaultMap = freshParams.flatten.map(v => v.name.toString -> v).toMap

      val lambdaBody = updatedBody match {
        case Term.Name(name) => Var(name, defaultMap.get(name).flatMap(_.displayType))
        case _ => Translucent(updatedBody, defaultMap)  
      }

      freshParams.foldRight[Expr](lambdaBody) { (params, acc) => AbsN(params, acc) }
    }

    private def buildFromAnonFunctionTerm(func: Term.AnonymousFunction): Expr = {
      val namedParams = mutable.ListBuffer.empty[Var]

      // This assumes an in-order traversal, although I'm not aware if this is guaranteed
      val transformedBody = func.transform {
        case Term.Ascribe(Term.Placeholder(), tpe) =>
          val namedParam = Var.fresh(Some("_p"), Some(tpe))
          namedParams += namedParam
          namedParam.term
        case _: Term.Placeholder =>
          val namedParam = Var.fresh(Some("_p"), None)
          namedParams += namedParam
          namedParam.term
      }.asInstanceOf[Term]

      val params = namedParams.toList
      val defaultMap = params.map(v => v.name.toString -> v).toMap

      val lambdaBody = transformedBody match {
        case Term.Name(name) => Var(name, defaultMap.get(name).flatMap(_.displayType))
        case _ => Translucent(transformedBody, defaultMap)  
      }

      AbsN(params, lambdaBody)
    }
  }
}
