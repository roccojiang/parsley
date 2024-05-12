package fix

import scala.meta._
import scala.PartialFunction.cond
import scalafix.v1._
import scalafix.lint.LintSeverity

import implicits.TreeOps

class AmbiguousImplicitConversions extends SyntacticRule("AmbiguousImplicitConversions") {
  override def fix(implicit doc: SyntacticDocument): Patch = {
    val orderedImports = doc.tree.collect { case i: Import => i }
    traverseImports(orderedImports)
  }

  private def traverseImports(imports: List[Import]): Patch = {
    @annotation.tailrec
    def visit(importsToVisit: List[Import], visitedImplicits: Seq[ImplicitConversion], patches: Patch): Patch = {
      importsToVisit match {
        // in Scala, import statements can appear anywhere, so we must take extra care here
        // walk left-to-right through each import statement (i.e. top-down when reading the source file)
        // this allows us to keep the scope lexically managed: imports are visited in order of appearance
        // so we correctly track which imports are in scope at any given point in the file
        case head :: tail =>
          val currentScope = head.parent.get // this should be fine, Imports should always have a parent

          val previousInScope = visitedImplicits.filter(i => currentScope.isWithinScope(i.importStat.parent.get))
          val currentInScope = previousInScope ++ getImplicitConversions(head)

          // if we already had clashing implicits in scope, don't report again
          if (!hasClashingImplicits(previousInScope) && hasClashingImplicits(currentInScope)) {
            visit(tail, currentInScope, patches + Patch.lint(AmbiguousImplicitConversionsLint(head, currentInScope)))
          } else {
            visit(tail, currentInScope, patches)
          }
        
        case Nil => patches
      }
    }

    visit(imports, Seq.empty, Patch.empty)
  }

  sealed abstract class ImplicitConversion extends Product with Serializable {
    def importStat: Import
    override def toString: String = s"${this.importStat} at line ${this.importStat.pos.startLine + 1}"
  }
  case class LexerImplicitSymbol(importStat: Import) extends ImplicitConversion
  case class ParsleyStringLift(importStat: Import) extends ImplicitConversion

  private def getImplicitConversions(importStat: Import): Seq[ImplicitConversion] = {
    def importsParsleyStringLift(importer: Importer): Boolean = cond(importer) {
      case Importer(Term.Select(Term.Select(Term.Name("parsley"), Term.Name("syntax")), Term.Name("character")), importees)
        if containsImportee(importees, Importee.Name(Name.Indeterminate("stringLift"))) ||
          containsImportee(importees, Importee.Wildcard()) => true
    }

    def mayImportLexerImplicit(importer: Importer): Boolean =
      containsImportee(importer.importees, Importee.Name(Name.Indeterminate("implicitSymbol"))) || (
        importer.ref.containsAnyOf(Term.Name("lexer")) &&
        importer.ref.containsAnyOf(Term.Name("implicit"), Term.Name("implicits"))
      )

    def containsImportee(importees: List[Importee], importee: Importee): Boolean =
      importees.exists(i => cond(i) {
        case _: Importee if i.structure == importee.structure => true
      })

    importStat.importers.collect {
      case i: Importer if mayImportLexerImplicit(i) => LexerImplicitSymbol(importStat)
      case i: Importer if importsParsleyStringLift(i) => ParsleyStringLift(importStat)
    }
  }

  private def hasClashingImplicits(implicits: Seq[ImplicitConversion]): Boolean = {
    implicits.exists(_.isInstanceOf[LexerImplicitSymbol]) && implicits.exists(_.isInstanceOf[ParsleyStringLift])
  }

  case class AmbiguousImplicitConversionsLint(tree: Tree, implicits: Seq[ImplicitConversion]) extends Diagnostic {
    override def position: Position = tree.pos
    override def severity: LintSeverity = LintSeverity.Error
    override def message: String =
      s"""There may be multiple, clashing implicit conversions in this scope:
         |* ${implicits.mkString("\n* ")}
         |If this is the case, you may encounter confusing errors like 'value/method is not a member of String/Char'.
         |To fix this, ensure that you only import a single implicit conversion.
       """.stripMargin
      // TODO: add a canonical link to the wiki page? e.g. For more information, see: https://j-mie6.github.io/parsley/5.0/api-guide/syntax.html.
  }
}

object implicits {
  implicit class TreeOps(val tree: Tree) extends AnyVal {
    def containsAnyOf(terms: Term*): Boolean = tree.collect {
      case t: Term if terms.toSeq.exists(term => t.structure == term.structure) => t
    }.nonEmpty

    def isWithinScope(scope: Tree): Boolean = {
      tree.structure == scope.structure ||
      tree.parent.map(_.isWithinScope(scope)).getOrElse(false)
    }
  }
}