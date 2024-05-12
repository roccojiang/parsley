package fix

import scala.meta._
import scalafix.v1._
import scalafix.lint.LintSeverity

import scala.PartialFunction.cond

class AmbiguousImplicitConversions extends SyntacticRule("AmbiguousImplicitConversions") {
  override def fix(implicit doc: SyntacticDocument): Patch = {
    val scopeTree = buildScopeTree(doc.tree)
    traverseScopeTree(scopeTree)
  }

  def traverseScopeTree(scopeTree: ScopeTree): Patch = {
    def recurse(currentScope: ScopeTree, implicits: Set[ImplicitConversion]): Patch = {
      currentScope match {
        case ImportScope(tree, imports, children) =>
          val newImplicits = imports.collect {
            case importStat @ Import(importers) => importers.collect {
              case i: Importer if mayImportLexerImplicit(i) => LexerImplicitSymbol(importStat)
              case Importer(Term.Select(Term.Select(Term.Name("parsley"), Term.Name("syntax")), Term.Name("character")), _) => ParsleySyntaxCharacterImplicits(importStat)
            }
          }.flatten.toSet // ++ implicits

          if (newImplicits.size > 1) {
            // println(s"Found ambiguous implicits in scope $currentScope: $newImplicits")
            Patch.lint(AmbiguousImplicitConversionsLint(tree, newImplicits))
          } else {
            children.map(recurse(_, newImplicits)).asPatch
          }

        case EmptyScope => Patch.empty
      }
    }

    recurse(scopeTree, Set.empty)
  }

  case class AmbiguousImplicitConversionsLint(tree: Tree, implicits: Set[ImplicitConversion]) extends Diagnostic {
    override def position: Position = tree.pos
    override def severity: LintSeverity = LintSeverity.Error
    override def message: String =
      s"""There may be multiple, clashing implicit conversions in this scope:
         |* ${implicits.mkString("\n* ")}
         |If this is the case, you may encounter confusing errors like 'value/method is not a member of String/Char'.
         |To fix this, ensure that you only import a single implicit conversion.
       """.stripMargin // TODO: is there a canonical link to the wiki page
      // TODO: add a canonical link to the wiki page? e.g. For more information, see: https://j-mie6.github.io/parsley/5.0/api-guide/syntax.html.
  }

  sealed abstract class ImplicitConversion extends Product with Serializable {
    def importStat: Import

    override def toString: String = s"${this.importStat} at line ${this.importStat.pos.startLine + 1}"
  }
  case class LexerImplicitSymbol(importStat: Import) extends ImplicitConversion
  case class ParsleySyntaxCharacterImplicits(importStat: Import) extends ImplicitConversion
  
  
  sealed abstract class ScopeTree extends Product with Serializable {
    def +(imports: Seq[Import]): ScopeTree
  }
  case class ImportScope(originalTree: Tree, imports: Seq[Import], children: Seq[ScopeTree]) extends ScopeTree {
    override def +(imports: Seq[Import]): ScopeTree = ImportScope(originalTree, this.imports ++ imports, children)
  }
  case object EmptyScope extends ScopeTree {
    override def +(imports: Seq[Import]): ScopeTree = this
  }

  object ImportScopeTree {
    def unapply(tree: Stat): Option[Seq[Stat]] = tree match {
      // global scope
      case Source(stats) => Some(stats)
      case Pkg(_, stats) => Some(stats)

      // package objects
      case Pkg.Object(_, _, Template.After_4_4_0(_, _, _, stats, _)) => Some(stats)
      // objects
      case Defn.Object(_, _, Template.After_4_4_0(_, _, _, stats, _)) => Some(stats)
      // classes
      case Defn.Class.After_4_6_0(_, _, _, _, Template.After_4_4_0(_, _, _, stats, _)) => Some(stats)
      // traits
      case Defn.Trait.After_4_6_0(_, _, _, _, Template.After_4_4_0(_, _, _, stats, _)) => Some(stats)
      // enums (scala 3)
      case Defn.Enum.After_4_6_0(_, _, _, _, Template.After_4_4_0(_, _, _, stats, _)) => Some(stats)
      // def function objects
      case Defn.Def.After_4_7_3(_, _, _, _, Term.Block(stats)) => Some(stats)

      case _ => None
    }
  }

  private def buildScopeTree(tree: Tree): ScopeTree = {
    tree match {
      // find the actual global scope, if nested in the top-level structure
      case Source(Seq(p: Pkg)) => buildScopeTree(p)
      case Pkg(_, Seq(p: Pkg)) => buildScopeTree(p)

      // standalone import statement - this happens when the import is in global scope, but not at the top of the file
      case i: Import => ImportScope(tree, Seq(i), Seq.empty)

      case ImportScopeTree(stats) =>
        val (importStats, otherStats) = stats.span(_.is[Import]) // imports are read top-down, so we must use span instead of partition
        val currentScopeImports = importStats.collect { case i: Import => i }
        ImportScope(tree, currentScopeImports, otherStats.map(buildScopeTree(_) + currentScopeImports))

      case _ => EmptyScope
    }
  }

  private def mayImportLexerImplicit(importer: Importer): Boolean = {
    def containsAny(parent: Tree, terms: Term*): Boolean = {
      parent.collect {
        case t: Term if terms.toSeq.exists(term => t.structure == term.structure) => t
      }.nonEmpty
    }

    def containsImportee(importees: List[Importee], importee: Importee): Boolean = {
      importees.exists(i => cond(i) {
        case _: Importee if i.structure == importee.structure => true
      })
    }

    containsImportee(importer.importees, Importee.Name(Name.Indeterminate("implicitSymbol"))) || (
      containsAny(importer.ref, Term.Name("lexer")) &&
      containsAny(importer.ref, Term.Name("implicit"), Term.Name("implicits"))
    )
  }
}
