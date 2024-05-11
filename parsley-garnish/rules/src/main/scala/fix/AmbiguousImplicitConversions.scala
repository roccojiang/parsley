package fix

import scala.meta._
import scalafix.v1._
import scalafix.lint.LintSeverity

class AmbiguousImplicitConversions extends SyntacticRule("AmbiguousImplicitConversions") {
  override def fix(implicit doc: SyntacticDocument): Patch = {
    // val globalImports = collectGlobalImports(doc.tree)
    // println(s"globalImports: $globalImports")

    // val localImports = collectLocalImports(doc.tree)
    // println("localImports:")
    // localImports.foreach(println)

    // val imports = collectImportsPerScope(doc.tree)
    // imports.foreach(i => println(i + "\n"))

    val scopeTree = collectImports(doc.tree)
    traverseScopeTree(scopeTree)
  
    // doc.tree.collect {
    //   case i @ Importer(Term.Select(Term.Select(Term.Name("parsley"), Term.Name("syntax")), Term.Name("character")), _) => println(s"parsley.syntax.character import: ${i.structure}")

    //   case i: Importer if mayImportLexerImplicit(i) => println(s"lexer.implicits import: ${i.structure}")
    // }

    // Patch.empty
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
          }.flatten.toSet ++ implicits

          if (newImplicits.size > 1) {
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
    override def message: String = s"Conflicting implicit conversions detected in this scope: $implicits"
  }

  sealed abstract class ImplicitConversion extends Product with Serializable
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
    def unapply(tree: Tree): Option[Seq[Stat]] = tree match {
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

  private def collectImports(tree: Tree): ScopeTree = {
    tree match {
      // find the actual global scope, if nested in the top-level structure
      case Source(Seq(p: Pkg)) => collectImports(p)
      case Pkg(_, Seq(p: Pkg)) => collectImports(p)

      case ImportScopeTree(stats) => ImportScope(tree, extractImports(stats), stats.map(collectImports(_)))

      case _ => EmptyScope
    }
  }

  private def extractImports(stats: Seq[Stat]): Seq[Import] = {
    stats.filter(_.is[Import]).collect { case i: Import => i }
  }

  private def mayImportLexerImplicit(importer: Importer): Boolean = {
    def containsAnyTerms(parent: Tree, terms: Term*): Boolean = {
      parent.collect {
        case t: Term if terms.toSeq.exists(term => t.structure == term.structure) => true
      }.nonEmpty
    }

    def containsImportee(importees: List[Importee], importee: Importee): Boolean = {
      importees.collect {
        case i: Importee if i.structure == importee.structure => true
      }.nonEmpty
    }

    containsImportee(importer.importees, Importee.Name(Name.Indeterminate("implicitSymbol"))) || (
      containsAnyTerms(importer.ref, Term.Name("lexer")) &&
      containsAnyTerms(importer.ref, Term.Name("implicit"), Term.Name("implicits"))
    )
  }
}
