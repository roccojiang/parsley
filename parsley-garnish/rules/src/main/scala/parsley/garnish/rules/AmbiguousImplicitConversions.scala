package parsley.garnish.rules

import scala.PartialFunction.cond
import scala.meta._
import scalafix.lint.LintSeverity
import scalafix.v1._

import parsley.garnish.implicits.TreeOps

class AmbiguousImplicitConversions extends SyntacticRule("AmbiguousImplicitConversions") {
  override def fix(implicit doc: SyntacticDocument): Patch = {
    val orderedImports = doc.tree.collect { case i: Import => i }
    traverseImports(orderedImports)
  }

  private def traverseImports(imports: List[Import]): Patch = {
    @annotation.tailrec
    def visit(importsToVisit: List[Import], visitedImports: Seq[Import], patches: Patch): Patch = {
      importsToVisit match {
        // in Scala, import statements can appear anywhere, so we must take extra care here
        // walk left-to-right through each import statement (i.e. top-down when reading the source file)
        // this allows us to keep the scope lexically managed: imports are visited in order of appearance
        // so we correctly track which imports are in scope at any given point in the file
        case head :: tail =>
          val currentScope = head.parent.get // this should be fine, Imports should always have a parent

          val alreadyInScope = visitedImports.filter(i => currentScope.isWithinScope(i.parent.get))
          val currentInScope = alreadyInScope :+ head
          val clashingImports = getClashingImports(currentInScope)

          // if we already had clashing implicits in scope, don't report again
          if (getClashingImports(alreadyInScope).isEmpty && clashingImports.nonEmpty) {
            visit(tail, currentInScope, patches + Patch.lint(AmbiguousImplicitConversionsLint(head, clashingImports)))
          } else {
            visit(tail, currentInScope, patches)
          }

        case Nil => patches
      }
    }

    visit(imports, Seq.empty, Patch.empty)
  }

  private def getClashingImports(imports: Seq[Import]): Seq[Import] = {
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

    val importers = imports.flatMap(_.importers)
    val parsleyStringLiftImports = importers.filter(importsParsleyStringLift)
    val lexerImplicitImports = importers.filter(mayImportLexerImplicit)

    if (parsleyStringLiftImports.nonEmpty && lexerImplicitImports.nonEmpty) {
      imports.collect {
        case i if i.importers.exists(importsParsleyStringLift) => i
        case i if i.importers.exists(mayImportLexerImplicit) => i
      }
    }
    else Seq.empty
  }

  case class AmbiguousImplicitConversionsLint(tree: Tree, imports: Seq[Import]) extends Diagnostic {
    override def position: Position = tree.pos
    override def severity: LintSeverity = LintSeverity.Warning
    override def message: String =
      s"""This import may cause clashing implicit conversions:
         |* ${imports.map(printImport).mkString("\n* ")}
         |If this is the case, you may encounter confusing errors like 'method is not a member of String'.
         |To fix this, ensure that you only import a single implicit conversion.
       """.stripMargin
    
    private def printImport(importStat: Import): String =
      s"${importStat} at line ${importStat.pos.startLine + 1}"
  }
}
