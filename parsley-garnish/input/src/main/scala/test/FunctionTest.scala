/* 
rule = SimplifyComplexParsers
 */
package test

import parsley.Parsley._
import parsley.character._
import parsley.lift._
import parsley.syntax.lift._
import parsley.syntax.zipped._
import parsley.generic._

// * map, lift (implicit and explicit), zipped, (.as perhaps?)
//   -- these should surely boil down into two cases: (x, y).xxx(f) and xxx(f, x, y)
//   * named function literals (val)
//   * named method literals (def)
//   * anonymous functions i.e. lambdas
//   * functions with placeholder syntax
//   * apply methods of case classes - symbol will tell its a class signature so we use this as a clue to look at synthetics???
// * generic bridges -- I reckon the information will probably show up in synthetics again

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

  def defFunc(x: String, y: String) = x + y
  def defFuncGeneric[A, B](x: A, y: B) = (x, y)
  def defFuncGenericCurried[A, B](x: A)(y: B) = (x, y)

  // val mapValFunc = pure("parsley").map(valFunc.curried)
  // val mapDefFunc = pure("parsley").map(defFunc.curried)
  // val mapDefFuncGeneric = pure("parsley").map(defFuncGenericCurried)
  // val mapPlaceholder = pure("parsley").map(_ + "garnish")
  // val mapLambda = pure("parsley").map(x => x + "garnish")
  // val mapApplyMethodLambda = pure("parsley").map(x => One(x.length))
  // val mapApplyMethodPlaceholder = pure("parsley").map(OneGeneric(_))

  // val explicitLiftValFunc = lift2(valFunc, pure("parsley"), pure("garnish"))
  // val explicitLiftDefFunc = lift2(defFunc, pure("parsley"), pure("garnish"))
  // val explicitLiftDefFuncGeneric = lift2(defFuncGeneric[String, String], pure("parsley"), pure("garnish"))
  // val explicitLiftPlaceholder = lift2((_: String) + (_: String), pure("parsley"), pure("garnish"))
  // val explicitLiftLambda = lift2((a: String, b: String) => a + b, pure("parsley"), pure("garnish"))
  // val explicitLiftApplyMethodLambda = lift2((a: String, b: String) => TwoGeneric(a, b), pure("parsley"), pure("garnish"))
  // val explicitLiftApplyMethodPlaceholder = lift2(Two(_, _), pure(1), pure(2))

  // val implicitLiftValFunc = valFunc.lift(pure("parsley"), pure("garnish"))
  // val implicitLiftDefFunc = defFunc.lift(pure("parsley"), pure("garnish"))
  // val implicitLiftDefFuncGeneric = defFuncGeneric[String, String].lift(pure("parsley"), pure("garnish"))
  // val implicitLiftPlaceholder = ((_: String) + (_: String)).lift(pure("parsley"), pure("garnish"))
  // val implicitLiftLambda = ((a: String, b: String) => a + b).lift(pure("parsley"), pure("garnish"))
  // val implicitLiftApplyMethodLambda = ((a: String, b: String) => TwoGeneric(a, b)).lift(pure("parsley"), pure("garnish"))
  // val implicitLiftApplyMethod = Two.lift(pure(1), pure(2))

  // val zippedValFunc = (pure("parsley"), pure("garnish")).zipped(valFunc)
  // val zippedDefFunc = (pure("parsley"), pure("garnish")).zipped(defFunc)
  // val zippedDefFuncGeneric = (pure("parsley"), pure("garnish")).zipped(defFuncGeneric)
  // val zippedPlaceholder = (pure("parsley"), pure("garnish")).zipped(_ + _)
  // val zippedLambda = (pure("parsley"), pure("garnish")).zipped((a, b) => a + b)
  // val zippedApplyMethodLambda = (pure("parsley"), pure("garnish")).zipped((a, b) => TwoGeneric(a, b))
  // val zippedApplyMethod = (pure(1), pure(2)).zipped(Two)
  // val zippedApplyMethodPlaceholder = (pure(1), pure(2)).zipped(Two(_, _))

  val bridge1 = OneBridged(pure(1))
  val bridge2 = TwoBridged(pure(1), pure(2))
  val bridgeFrom = OneBridged.from(pure(1))
  val bridgeFromInfix = OneBridged from pure(1)


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
