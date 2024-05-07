package fix

import parsley.Parsley, parsley.Parsley._
import parsley.character._
import parsley.lift._
import parsley.syntax.lift._
import parsley.syntax.zipped._
import parsley.expr.chain

object SyntheticsTest {
  sealed trait Expr
  case class Add(a: Expr, b: Expr) extends Expr
  case class Wow(a: Expr, b: Expr, c: Expr) extends Expr
  case class Atom(a: String) extends Expr

  lazy val r: Parsley[Expr] = chain.postfix[Expr](string("b").map(Atom))(string("a").map(fresh22 => Atom((fresh19: Expr) => Add.curried(fresh19)(fresh22))))


  case class One(a: String)
  case class OneGeneric[A](a: A)
  case class Double(a: String, b: Int)
  case class DoubleGeneric[A, B](a: A, b: B)
  case class DoubleCurried(a: String)(b: Int)
  case class DoubleGenericCurried[A, B](a: A)(b: B)
  case class TripleGenericPartialCurried[A, B](a: A)(b: Boolean, c: B)

  // val implicitLift = DoubleGeneric[Int, String].lift(pure(1), string("a"))

  // val explicitLift = lift2(DoubleGeneric[Int, String](_, _), pure(1), string("a"))
  
  // val zipped = (pure(1), string("a")).zipped(DoubleGeneric(_, _))
  // val zippedCurried = (string("a"), pure(1)).zipped(DoubleCurried(_)(_))
  // val zippedLambda = (pure(1), string("a")).zipped((x, y) => DoubleGeneric(x, y))
  
  // val map = string("a").map(One(_))
  // val mapGeneric = string("a").map(OneGeneric(_))
  // val mapCurried = string("a").map(DoubleCurried(_)(1))
  // val mapGenericCurried = string("a").map(DoubleGenericCurried(_)(1))

  // val mapTriple = string("a").map(TripleGenericPartialCurried(1)(false, _))
  // val mapTripleLambda = string("a").map(x => TripleGenericPartialCurried(1)(false, x))

  // def id[A](a: A): A = a
  // val mapFunc = string("a").map(id)

  // Partial functions not supported! Will likely cause a broken output
  // val mapDouble = (string("a") <~> pure(1)).map { case (s, i) => DoubleSpecific(s, i) }
}
