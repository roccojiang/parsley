package test

import parsley.Parsley._
import parsley.character._
import parsley.lift._
import parsley.syntax.lift._
import parsley.syntax.zipped._
import parsley.generic._

// * map, lift (implicit and explicit), zipped, (.as perhaps?)
//   * named function literals (val)
//   * named method literals (def)
//   * anonymous functions i.e. lambdas
//   * functions with placeholder syntax
//   * apply methods of case classes - symbol will tell its a class signature so we use this as a clue to look at synthetics???
// * generic bridges -- I reckon the information will probably show up in synthetics again
// so overall, we have shapes (x, y).xxx(f) ; xxx(f, x, y) ; f.xxx(x, y) ; f(x, y)
//  hopefully this won't matter though, as I want to only need to use term `f`, without needing surrounding contextual information

object FunctionTest {
  case class One(x: Int)
  case class Two(x: Int, y: Int)

  case class OneGeneric[A](a: A)
  case class TwoGeneric[A, B](a: A, b: B)

  case class OneBridged(x: Int)
  object OneBridged extends ParserBridge1[Int, OneBridged]
  case class TwoBridged(x: Int, y: Int)
  object TwoBridged extends ParserBridge2[Int, Int, TwoBridged]

  val valFunc = (x: String, y: String) => x + y
  val valFuncCurried = (x: String) => (y: String) => x + y

  def defFunc(x: String, y: String) = x + y
  def defFuncCurried = defFunc.curried
  def defFuncGeneric[A, B](x: A, y: B) = (x, y)
  def defFuncGenericCurried[A, B](x: A)(y: B) = (x, y)

  /* WONT DO: can't extract concrete type of function from signature, and there are no synthetics */
  // val mapValFunc = pure("parsley").map(valFunc.curried)

  // TODO: named functions - this is just a Term.Name("func_name"), with symbols
  val mapValFunc = pure(valFuncCurried("parsley"))
  val mapDefFunc = pure(defFuncCurried("parsley"))
  val mapDefFuncGeneric = pure(defFuncGenericCurried("parsley"))

  /* 
  LIKELY WONT DO: it seems impossible to find the type of placeholder variables
  TODO: But we can at least get the shape of the function
  */
  val mapPlaceholder = pure("parsley" + "garnish")

  /*
  TODO: is this any different from how it would work for x => One(x.length)? Don't think so
  We can find the type of each argument via its signature
  And the return type possibly via synthetics on the .map term?
  Also possible to get signature from symbol of method body, if the types are concrete
    (if it's in a lambda like this, it should always be?)
  */
  val mapLambda = pure("parsley".toInt + 12)

  // TODO: standard anonymous functions (i.e. lambdas) - Term.Function(ParamClause(params), body)
  // get argument types from ParamClause
  // recursively descend into body, in case the function is curried
  // get return type from final body
  val mapApplyMethodLambda = pure(One("parsley".length))

  // I the below should end up parsed like \x -> \y -> OPAQUE(One(x.length + y))
  // it's NOT worth trying to turn the body of the lambda into our Function representation
  val mapApplyMethodLambdaCurried = pure("parsley").map(x => (y: Int) => One(x.length + y))

  // TODO: Need to have some sort of traversal through and convert this to a lambda
  // This is a Term.AnonymousFunction(...)
  val mapApplyMethodPlaceholder = pure(OneGeneric("parsley"))
  val mapApplyMethodPlaceholderLabelledType = pure(OneGeneric("parsley": String))

  val mapApplyMethod = pure(Two.curried.apply(1))

  val explicitLiftValFunc = lift2(valFunc, pure("parsley"), pure("garnish"))
  val explicitLiftDefFunc = lift2(defFunc, pure("parsley"), pure("garnish"))
  val explicitLiftDefFuncGeneric = lift2(defFuncGeneric[String, String], pure("parsley"), pure("garnish"))
  val explicitLiftPlaceholder = lift2((_: String) + (_: String), pure("parsley"), pure("garnish"))
  val explicitLiftLambda = lift2((a: String, b: String) => a + b, pure("parsley"), pure("garnish"))
  val explicitLiftApplyMethodLambda = lift2((a: String, b: String) => TwoGeneric(a, b), pure("parsley"), pure("garnish"))
  val explicitLiftApplyMethodPlaceholder = lift2(Two(_, _), pure(1), pure(2))
  val explicitLiftApplyMethod = lift2(Two, pure(1), pure(2))

  // val implicitLiftValFunc = valFunc.lift(pure("parsley"), pure("garnish"))
  // val implicitLiftDefFunc = defFunc.lift(pure("parsley"), pure("garnish"))
  // val implicitLiftDefFuncGeneric = defFuncGeneric[String, String].lift(pure("parsley"), pure("garnish"))
  // val implicitLiftPlaceholder = ((_: String) + (_: String)).lift(pure("parsley"), pure("garnish"))
  // val implicitLiftLambda = ((a: String, b: String) => a + b).lift(pure("parsley"), pure("garnish"))
  // val implicitLiftApplyMethodLambda = ((a: String, b: String) => TwoGeneric(a, b)).lift(pure("parsley"), pure("garnish"))
  // val implicitLiftApplyMethod = TwoGeneric[Int, Int].lift(pure(1), pure(2))

  // val zippedValFunc = (pure("parsley"), pure("garnish")).zipped(valFunc)
  // val zippedDefFunc = (pure("parsley"), pure("garnish")).zipped(defFunc)
  // val zippedDefFuncGeneric = (pure("parsley"), pure("garnish")).zipped(defFuncGeneric)
  // val zippedPlaceholder = (pure("parsley"), pure("garnish")).zipped(_ + _)
  // val zippedLambda = (pure("parsley"), pure("garnish")).zipped((a, b) => a + b)
  // val zippedApplyMethodLambda = (pure("parsley"), pure("garnish")).zipped((a, b) => TwoGeneric(a, b))
  // val zippedApplyMethod = (pure(1), pure(2)).zipped(Two)
  // val zippedApplyMethodExplicit = (pure(1), pure(2)).zipped(Two.apply)
  // val zippedApplyMethodPlaceholder = (pure(1), pure(2)).zipped(Two(_, _))

  // val bridge1 = OneBridged(pure(1))
  // val bridge2 = TwoBridged(pure(1), pure(2))

  /*
  val bridgeFrom = OneBridged.from(pure(1))
  val bridgeFromInfix = OneBridged from pure(1)
  */


  /*
  val complexLambda = item.map(x => {
    if (x.isDigit) One(x.asDigit)
    else OneGeneric(x)
  })

  val nameShadowingComplexLambda = item.map(x => {
    if (x.isDigit) x => One(x) // in the returned function, x does not refer to the outer lambda's x
    else (y: Int) => OneGeneric(y)
  })
  */
}
