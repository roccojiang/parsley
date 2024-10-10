package parsley.garnish.utils

import scala.meta._
import scalafix.v1._

object PatchUtils {
  def addPatchAbove(tree: Tree, toAdd: String)(implicit doc: SemanticDocument): Patch = {
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
