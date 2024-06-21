package parsley.garnish.expr

import scala.collection.mutable
import scala.meta._
import scalafix.v1._

import parsley.garnish.expr.Expr, Expr._
import parsley.garnish.expr.TypeSignatureAnalyzer.getInferredTypeSignature

object ExprLifter {

  def lift(term: Term, numParams: Int)(implicit doc: SemanticDocument): Expr = term match {   
    case f: Term.Function          => buildFromFunctionTerm(f)
    case f: Term.AnonymousFunction => buildFromAnonFunctionTerm(f)
    case f: Term.Name if numParams > 0 =>
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
