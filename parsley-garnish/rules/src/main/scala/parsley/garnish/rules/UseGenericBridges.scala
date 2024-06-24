package parsley.garnish.rules

import scala.collection.mutable
import scala.meta._
import scalafix.v1._

import parsley.garnish.expr.Expr, Expr._
import parsley.garnish.parser.Parser, Parser._
import parsley.garnish.parser.GrammarExtractor.{getParserDefinitions, ParserDefinition}
import parsley.garnish.utils.PatchUtils.addPatchAbove

/**
  * Experimental rule to convert map/lift/zipped parsers to use generic bridges.
  * 
  * Limitations:
  *  - Assumes that a constructor class exists.
  *  - Adds the ParserBridgeN companion object to the file where the parser is defined. If the constructor class is
  *    defined in a different file, the user will have to move the companion object to the correct file.
  *  - Sometimes the type of the parser bridge cannot be inferred, and will be replaced with ???. The user will have
  *    to fill in these types manually.
  */
class UseGenericBridges extends SemanticRule("UseGenericBridges") {
  override def fix(implicit doc: SemanticDocument): Patch = {
    val bridgePatches = getParserDefinitions().map { case ParserDefinition(_, parser, _, originalTree) =>
      implicit val companionObjectsToCreate = mutable.LinkedHashSet.empty[String] // maintain ordering

      val updatedParser = parser.transform(changeToBridge)

      if (!parser.isEquivalent(updatedParser)) {
        val parserRewrite = Patch.replaceTree(originalTree, updatedParser.term.syntax)
        val companionObjectPatches = companionObjectsToCreate.map(addPatchAbove(originalTree, _)).asPatch

        (parserRewrite + companionObjectPatches).atomic
      } else {
        Patch.empty
      }
    }.asPatch

    val imports = if (bridgePatches.nonEmpty) Patch.addGlobalImport(importer"parsley.generic._") else Patch.empty

    imports + bridgePatches
  }

  private case class BridgeArgs(constructor: Term, types: List[Type.Name])

  // If types cannot be inferred, replace with ??? and let the user fix it manually
  private def changeToBridge(implicit bridges: mutable.Set[String]): PartialFunction[Parser, Parser] = {
    // TODO: more precisely, f must be a constructor - apply method
    case FMap(p, AbsN(xs, AppN(f, ys))) if xs == ys =>
      bridges += buildCompanionObjectString(f.term, List(p.tpe.getOrElse(Type.Name("???"))))
      Bridge(f, List(p))

    case p: LiftParser => p.func match {
      case AbsN(xs, AppN(f, ys)) if xs == ys =>
        bridges += buildCompanionObjectString(f.term, p.parsers.map(_.tpe.getOrElse(Type.Name("???"))))
        Bridge(f, p.parsers)
      case _ => p
    }
  }

  private def buildCompanionObjectString(constructor: Term, types: List[Type.Name]): String = {
    val genericBridge = s"ParserBridge${types.length}"
    val typeArgs = s"${types.map(_.text.stripPrefix("`").stripSuffix("`")).mkString(", ")}, $constructor"
    s"object $constructor extends $genericBridge[$typeArgs]"
  }
}
