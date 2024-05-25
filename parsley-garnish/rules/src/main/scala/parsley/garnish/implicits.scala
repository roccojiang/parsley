package parsley.garnish

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
    import parsley.garnish.model.Parser, Parser._

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
  }
}
