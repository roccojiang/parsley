package parsley.garnish.rules

import scalafix.v1._

import parsley.garnish.parser.Parser, Parser._

class AvoidParserRedefinition extends SemanticRule("AvoidParserRedefinition") {
  override def fix(implicit doc: SemanticDocument): Patch = rewriteAllParsers(_.rewrite(redefinitions))

  def redefinitions: PartialFunction[Parser, Parser] = {
    case ManyP(p <~ sep) => EndBy(p, sep)
  }
}
