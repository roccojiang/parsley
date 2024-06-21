package parsley.garnish.rules

import scala.meta._
import scalafix.v1._

import parsley.garnish.parser.Parser, Parser.{Chr, Str}

class UseImplicitConversions extends SemanticRule("UseImplicitConversions") {

  override def fix(implicit doc: SemanticDocument): Patch = {
    val rewrites = rewriteAllParsers(_.transform(changeSyntax).etaReduceExprs)
    val imports = if (rewrites.nonEmpty) Patch.addGlobalImport(importer"parsley.syntax.character._") else Patch.empty

    rewrites + imports
  }

  def changeSyntax: PartialFunction[Parser, Parser] = {
    case Chr(c, _) => Chr(c, implicitSyntax = true)
    case Str(s, _) => Str(s, implicitSyntax = true)
  }
 }
