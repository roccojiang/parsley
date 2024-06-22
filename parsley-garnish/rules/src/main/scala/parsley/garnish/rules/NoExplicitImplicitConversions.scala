package parsley.garnish.rules

import scala.meta._
import scalafix.v1._

import parsley.garnish.utils.PatchUtils.addPatchAbove

class NoExplicitImplicitConversions extends SemanticRule("NoExplicitImplicitConversions") {

  private val implicitConv = SymbolMatcher.normalized(
    "parsley.syntax.character.charLift",
    "parsley.syntax.character.stringLift",
    "parsley.token.symbol.ImplicitSymbol.implicitSymbol"
  )

  override def fix(implicit doc: SemanticDocument): Patch = doc.tree.collect {
    case qualifiedApp @ Term.Apply.After_4_6_0(qual @ implicitConv(_: Term.Select), Term.ArgClause(List(liftedArg), _)) =>
      val removeExplicitCall = Patch.replaceTree(qualifiedApp, liftedArg.syntax)
      val importQualifiedName = addPatchAbove(qualifiedApp, s"import $qual")
      (removeExplicitCall + importQualifiedName).atomic

    case app @ Term.Apply.After_4_6_0(implicitConv(_), Term.ArgClause(List(liftedArg), _)) =>
      Patch.replaceTree(app, liftedArg.syntax).atomic
  }.asPatch
}
