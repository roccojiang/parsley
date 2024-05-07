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

  // lazy val r: Parsley[Expr] = Add.lift(r, string("a").map(a => Atom(a))) // TODO: converting to Func doesn't work with the lift, so this breaks the shape of the functions as it's just inferred as a Opaque(Add)
  // lazy val r: Parsley[Expr] = (r, string("a").map(a => Atom(a))).zipped((x, y) => Add(x, y)) | string("b").map(Atom(_))

  // lazy val s: Parsley[Expr] = Wow.lift(s, string("a").map(Atom(_)), string("b").map(Atom(_))) | string("c").map(Atom(_))
  lazy val s: Parsley[Expr] = chain.postfix[Expr](string("c").map((fresh6: String) => Atom(fresh6)))(string("a").map(fresh37 => (fresh33: Expr) => (fresh34: Expr) => Wow.curried(fresh34)(Atom(fresh37))(fresh33)) <*> string("b").map((fresh5: String) => Atom(fresh5)))

  // lazy val t: Parsley[Expr] = chain.postfix[Expr](empty)(string("a").map((fresh20 => fresh21 => fresh22 => fresh20(fresh21(fresh22)))((fresh3: String) => Atom(fresh3))((fresh17 => fresh18 => fresh19 => fresh17(fresh19)(fresh18))((fresh8 => fresh9 => fresh10 => fresh8(fresh9(fresh10)))((fresh1: Expr) => (fresh2: Expr) => Add(fresh1)(fresh2))(fresh4 => fresh4)))))

  // lazy val s: Parsley[Expr] = (s, pure(Atom("a")), pure(Atom("b"))).zipped((x, y, z) => Wow(x, y, z))

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
