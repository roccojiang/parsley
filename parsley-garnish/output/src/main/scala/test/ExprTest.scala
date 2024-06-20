package test

import parsley.Parsley._
import parsley.character._
import parsley.lift._
import parsley.syntax.lift._
import parsley.syntax.zipped._
import parsley.generic._

/**
  * A collection of scenarios where parsers take functions as arguments.
  * Used for debugging/testing the generic expression lifting functionality.
  */
object ExprTest {
  /*
   * In general, functions are found in the following positions:
     * (x, y).xxx(f)
     * xxx(f, x, y)
     * f.xxx(x, y)
     * f(x, y)
   */

  // TODO: partial functions not modelled in Expr type

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

  val mapValFuncCurried = pure(valFunc.curried("parsley"))
  val mapValFunc = pure(valFuncCurried("parsley"))
  val mapDefFunc = pure(defFuncCurried("parsley"))
  val mapDefFuncGeneric = pure(x1 => defFuncGenericCurried("parsley")(x1))
  val mapPlaceholder = pure("parsley" + "garnish")

  /*
  We can find the type of each argument via its signature
  And the return type possibly via synthetics on the .map term?
  Also possible to get signature from symbol of method body, if the types are concrete
    (if it's in a lambda like this, it should always be?)
  */
  val mapLambda = pure("parsley".toInt + 12)
  val mapApplyMethodLambda = pure(One("parsley".length))
  val mapApplyMethodLambdaCurried = pure((x1: Int) => One("parsley".length + x1))
  val mapApplyMethodPlaceholder = pure(OneGeneric("parsley"))
  val mapApplyMethodPlaceholderLabelledType = pure(OneGeneric("parsley"))
  val mapApplyMethod = pure(Two.curried.apply(1))

  val explicitLiftValFunc = lift2(valFunc, pure("parsley"), pure("garnish"))
  val explicitLiftDefFunc = lift2(defFunc, pure("parsley"), pure("garnish"))
  val explicitLiftDefFuncGeneric = lift2(defFuncGeneric[String, String], pure("parsley"), pure("garnish"))

  val explicitLiftPlaceholder = lift2((_: String) + (_: String), pure("parsley"), pure("garnish"))
  val explicitLiftLambda = lift2((a: String, b: String) => a + b, pure("parsley"), pure("garnish"))
  val explicitLiftApplyMethodLambda = lift2((a: String, b: String) => TwoGeneric(a, b), pure("parsley"), pure("garnish"))
  val explicitLiftApplyMethodPlaceholder = lift2(Two(_, _), pure(1), pure(2))
  val explicitLiftApplyMethod = lift2(Two, pure(1), pure(2))

  val implicitLiftValFunc = valFunc.lift(pure("parsley"), pure("garnish"))
  val implicitLiftDefFunc = defFunc.lift(pure("parsley"), pure("garnish"))
  val implicitLiftDefFuncGeneric = defFuncGeneric[String, String].lift(pure("parsley"), pure("garnish"))
  val implicitLiftPlaceholder = ((_: String) + (_: String)).lift(pure("parsley"), pure("garnish"))
  val implicitLiftLambda = ((a: String, b: String) => a + b).lift(pure("parsley"), pure("garnish"))
  val implicitLiftApplyMethodLambda = ((a: String, b: String) => TwoGeneric(a, b)).lift(pure("parsley"), pure("garnish"))
  val implicitLiftApplyMethod = TwoGeneric[Int, Int].lift(pure(1), pure(2))

  val zippedValFunc = (pure("parsley"), pure("garnish")).zipped(valFunc)
  val zippedDefFunc = (pure("parsley"), pure("garnish")).zipped(defFunc)
  val zippedDefFuncGeneric = (pure("parsley"), pure("garnish")).zipped(defFuncGeneric)
  val zippedPlaceholder = (pure("parsley"), pure("garnish")).zipped(_ + _)
  val zippedLambda = (pure("parsley"), pure("garnish")).zipped((a, b) => a + b)
  val zippedApplyMethodLambda = (pure("parsley"), pure("garnish")).zipped((a, b) => TwoGeneric(a, b))
  val zippedApplyMethod = (pure(1), pure(2)).zipped(Two)
  val zippedApplyMethodExplicit = (pure(1), pure(2)).zipped(Two.apply)
  val zippedApplyMethodPlaceholder = (pure(1), pure(2)).zipped(Two(_, _))

  val bridge1 = OneBridged(pure(1))
  val bridge2 = TwoBridged(pure(1), pure(2))
  val bridgeFrom = OneBridged.from(pure(1))
  val bridgeFromInfix = OneBridged from pure(1)

  val complexLambda = item.map(x => {
    if (x.isDigit) One(x.asDigit)
    else OneGeneric(x)
  })

  val nameShadowingComplexLambda = item.map(x => {
    if (x.isDigit) x => One(x) // in the returned function, x does not refer to the outer lambda's x
    else (y: Int) => OneGeneric(y)
  })
}
