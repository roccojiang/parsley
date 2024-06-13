/* 
rule = AvoidParserRedefinition
 */
package test

import parsley.Parsley._
import parsley.character._
import parsley.combinator._
import parsley.syntax.character._

object AvoidParserRedefinitionTest {
  val myEndBy = many("p" <~ "sep")
}
