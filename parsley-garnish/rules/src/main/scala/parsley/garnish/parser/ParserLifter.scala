package parsley.garnish.parser

import scala.PartialFunction.cond
import scala.meta._
import scalafix.v1._

import parsley.garnish.implicits.TermOps
import parsley.garnish.parser.Parser._
import parsley.garnish.utils.TypeUtils.{getParsleyType, isParsleyType}

object ParserLifter {

  def lift(term: Term)(implicit doc: SemanticDocument): Parser = term match {
    /* Core parsers (excluding non-terminals) */
    case Term.Apply.After_4_6_0(matchers.pure(_), Term.ArgClause(List(func), _)) =>
      Pure(func.toExpr())
    case matchers.empty(Term.Name(_)) =>
      Empty
    case Term.ApplyInfix.After_4_6_0(p, matchers.choice(_), _, Term.ArgClause(List(q), _)) =>
      p.toParser | q.toParser
    case Term.ApplyInfix.After_4_6_0(p, matchers.ap(_), _, Term.ArgClause(List(q), _)) =>
      p.toParser <*> q.toParser

    /* Lifting parsers */
    case Term.Apply.After_4_6_0(Term.Select(qual, matchers.map(_)), Term.ArgClause(List(func), _)) =>
      FMap(qual.toParser, func.toExpr())
    case Term.Apply.After_4_6_0(matchers.liftExplicit(_), Term.ArgClause(f :: ps, _)) =>
      LiftExplicit(f.toExpr(ps.size), ps.map(_.toParser))
    case Term.Apply.After_4_6_0(Term.Select(f, matchers.liftImplicit(_)), Term.ArgClause(ps, _)) =>
      val func = f match {
        case Term.ApplyType.After_4_6_0(g, _) => g
        case _ => f
      }
      LiftImplicit(func.toExpr(ps.size), ps.map(_.toParser))
    case Term.Apply.After_4_6_0(Term.Select(Term.Tuple(ps), matchers.zipped(_)), Term.ArgClause(List(f), _)) =>
      Zipped(f.toExpr(ps.size), ps.map(_.toParser))
    case Term.Apply.After_4_6_0(func, ps) if func.synthetics.exists(cond(_) {
        case SelectTree(_, IdTree(symInfo)) => matchers.bridge.matches(symInfo.symbol)
    }) =>
      Bridge(func.toExpr(ps.size), ps.map(_.toParser)) // directly mapping over the ArgClause without unpacking it seems to work fine

    /* Character parsers */
    case Term.Apply.After_4_6_0(matchers.string(_), Term.ArgClause(List(Lit.String(str)), _)) =>
      Str(str, implicitSyntax = false)
    case s @ Lit.String(str) if s.synthetics.exists(cond(_) {
      case ApplyTree(IdTree(symInfo), _) => matchers.stringLift.matches(symInfo.symbol)
    }) => Str(str, implicitSyntax = true)
    case Term.Apply.After_4_6_0(matchers.char(_), Term.ArgClause(List(Lit.Char(chr)), _)) =>
      Chr(chr, implicitSyntax = false)
    case s @ Lit.Char(chr) if s.synthetics.exists(cond(_) {
      case ApplyTree(IdTree(symInfo), _) => matchers.charLift.matches(symInfo.symbol)
    }) => Chr(chr, implicitSyntax = true)

    /* Sequencing parsers */
    case Term.ApplyInfix.After_4_6_0(p, matchers.then(_), _, Term.ArgClause(List(q), _)) =>
      p.toParser ~> q.toParser
    case Term.ApplyInfix.After_4_6_0(p, matchers.thenDiscard(_), _, Term.ArgClause(List(q), _)) =>
      p.toParser <~ q.toParser

    /* Iterative parsers */
    case Term.Apply.After_4_6_0(matchers.many(_), Term.ArgClause(List(p), _)) =>
      ManyP(p.toParser)
    case Term.Apply.After_4_6_0(matchers.some(_), Term.ArgClause(List(p), _)) =>
      SomeP(p.toParser)

    /* Separated values parsers */
    case Term.Apply.After_4_6_0(matchers.endBy(_), Term.ArgClause(List(p, sep), None)) =>
      EndBy(p.toParser, sep.toParser)

    /* Non-terminals */
    case t: Term.Name if !(t.symbol.owner.value startsWith "parsley/") =>
      NonTerminal(t.symbol, t.symbol.info.get.displayName)

    /* Otherwise, unknown parser */
    case unrecognised => Unknown(unrecognised)
  }

  type Grammar = Map[Symbol, ParserDefinition]

  final case class ParserDefinition(name: Term.Name, parser: Parser, tpe: Type.Name, originalTree: Term) {
    val definitionSite: Position = originalTree.parent.getOrElse(originalTree).pos
  }

  def getParserDefinitions(includeDefDefinitions: Boolean = false)
                          (implicit doc: SemanticDocument): Seq[ParserDefinition] = {
    val varDefns = doc.tree.collect {
      // See https://scalameta.org/docs/semanticdb/specification.html#symbol for symbol uniqueness guarantees
      // since we only deal with one document at a time, it should be fine to look at both global and local symbols
      case VariableDecl(vars, body) => collectVars(vars, body)
    }.flatten

    val defDefns = doc.tree.collect {
      case defDefn @ Defn.Def.After_4_7_3(_, name, _, _, body) if isParsleyType(defDefn.symbol) =>
        // TODO: do we need to do something special to deal with the function arguments?
        Seq(buildParserDefinition(defDefn.symbol, name, body))
    }.flatten

    varDefns ++ (if (includeDefDefinitions) defDefns else Seq.empty)
  }

  def getGrammarMap(includeDefDefinitions: Boolean = false)(implicit doc: SemanticDocument): Grammar =
    getParserDefinitions(includeDefDefinitions).map(parserDefn => parserDefn.name.symbol -> parserDefn).toMap

  private object VariableDecl {
    def unapply(tree: Tree): Option[(List[Pat], Term)] = tree match {
      case Defn.Val(_, vars, _, body)             => Some((vars, body))
      case Defn.Var.After_4_7_2(_, vars, _, body) => Some((vars, body))

      case _ => None
    }
  }

  private def collectVars(vars: List[Pat], body: Term)(implicit doc: SemanticDocument): Seq[ParserDefinition] =
    vars.collect {
      case Pat.Var(varName) if isParsleyType(varName.symbol) =>
        buildParserDefinition(varName.symbol, varName, body)
    }

  private def buildParserDefinition(sym: Symbol, name: Term.Name, body: Term)
                                   (implicit doc: SemanticDocument): ParserDefinition = {
    val tpe = getParsleyType(sym)
    assert(tpe.isDefined, s"expected a Parsley type for $name, got ${sym.info.get.signature}")
    ParserDefinition(name, body.toParser, tpe.get, body)
  }
}
