package parsley.garnish.rules

import scala.meta._
import scalafix.v1._
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

  private def addPatchAbove(tree: Tree, toAdd: String)(implicit doc: SemanticDocument): Patch = {
    import scalafix.util.TokenList

    val globalTokens = TokenList(doc.tree.tokens)
    // Find the first newline token before the given tree
    val loc = globalTokens.leading(tree.tokens.head).find(_.is[Token.LF]).get
    // Find the leading spaces between the newline and the first non-space token on that line
    // Drop the newline token afterwards, because I can't figure out how to work with token indices
    val leadingSpaces = globalTokens.slice(loc, tree.tokens.head).drop(1).takeWhile(_.is[Token.Space]).mkString
    Patch.addRight(loc, s"$leadingSpaces$toAdd\n")
  }
}
