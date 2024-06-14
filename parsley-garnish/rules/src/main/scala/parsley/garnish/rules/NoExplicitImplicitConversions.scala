package parsley.garnish.rules

import scala.meta._
import scalafix.v1._
import scalafix.util.TokenList

class NoExplicitImplicitConversions extends SemanticRule("NoExplicitImplicitConversions") {

  private val implicitConv = SymbolMatcher.normalized(
    "parsley.syntax.character.charLift",
    "parsley.syntax.character.stringLift",
    "parsley.token.symbol.ImplicitSymbol.implicitSymbol"
  )

  override def fix(implicit doc: SemanticDocument): Patch = doc.tree.collect {
    case qualifiedApp @ Term.Apply.After_4_6_0(qual @ implicitConv(_: Term.Select), Term.ArgClause(List(liftedArg), _)) =>
      val importPos = getImportInsertionPoint(qualifiedApp).getOrElse(doc.tree)

      val removeExplicitCall = Patch.replaceTree(qualifiedApp, liftedArg.syntax)
      val importQualifiedName = addPatchAbove(importPos, s"import $qual")
      (removeExplicitCall + importQualifiedName).atomic

    case app @ Term.Apply.After_4_6_0(implicitConv(_), Term.ArgClause(List(liftedArg), _)) =>
      Patch.replaceTree(app, liftedArg.syntax).atomic
  }.asPatch

  private def addPatchAbove(tree: Tree, toAdd: String)(implicit doc: SemanticDocument): Patch = {
    val globalTokens = TokenList(doc.tree.tokens)
    val leadingSpaces = globalTokens.leadingSpaces(tree.tokens.head).mkString
    Patch.addLeft(tree, s"$toAdd\n$leadingSpaces")
  }

  private def getImportInsertionPoint(term: Term.Apply): Option[Tree] = term.parent.flatMap(_.parent)
  
  private def getLastImport(term: Term.Apply): Option[Tree] = {
    val scope = term.parent.flatMap(_.parent)
    val lastImport = scope.flatMap(_.parent.collect { case i: Import => i }.lastOption)
    lastImport.orElse(scope)
  }
}
