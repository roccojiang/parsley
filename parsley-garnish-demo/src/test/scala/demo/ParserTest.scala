package demo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import parsley.errors.ErrorBuilder

import demo.parser._

class ParserTest extends AnyFlatSpec {
  def parse[Err: ErrorBuilder](input: String): Either[Err, Prog] = parser.parse(input).toEither

  "prog" should "be able to parse no assignments" in {
    parse("13") shouldBe Right(Prog(Nil, Val(13)))
    parse("x") shouldBe Right(Prog(Nil, Var("x")))
  }

  it should "be able to parse assignments along with one expression" in {
    parse("let v = 34; let y = v; y") shouldBe
      Right(Prog(List(Asgn("v", Val(34)), Asgn("y", Var("v"))), Var("y")))
  }

  it should "parse matching brackets" in {
    parse("let x = ((10)); ((((x))))") shouldBe
      Right(Prog(List(Asgn("x", Val(10))), Var("x")))
  }

  it should "reject missing terminal expression" in {
    parse("let v = 34;") shouldBe a[Left[_, _]]
  }

  it should "reject missing semicolons" in {
    // Unexpected(Some(Token.Nat(5)), "semicolon")
    // UnparsedInput
    parse("let v = 34 5") shouldBe a[Left[_, _]]
  }

  it should "reject mismatched brackets" in {
    parse("let x = 5); 2") shouldBe a[Left[_, _]]
    parse("let x = (5; 2") shouldBe a[Left[_, _]]
    parse("let x = 5; )") shouldBe a[Left[_, _]]
    parse("let x = 5; (") shouldBe a[Left[_, _]]
  }

  it should "reject parenthesised assignments" in {
    parse("(let x = 5); 2") shouldBe a[Left[_, _]]
  }

  it should "parse correct precedence for +/*" in {
    parse("4 * y + 5 * x") shouldBe Right(
      Prog(Nil, Add(Mul(Val(4), Var("y")), Mul(Val(5), Var("x"))))
    )
  }
  it should "allow parentheses to override" in {
    parse("(4 + y) * (5 + x)") shouldBe Right(
      Prog(Nil, Mul(Add(Val(4), Var("y")), Add(Val(5), Var("x"))))
    )
  }

  "expr" should "parse additions in a left-associative way" in {
    parse("x + 5 + z") shouldBe Right(
      Prog(Nil, Add(Add(Var("x"), Val(5)), Var("z")))
    )
  }

  it should "parse right-associatively in the presence of parentheses" in {
    parse("x + (5 + z)") shouldBe Right(
      Prog(Nil, Add(Var("x"), Add(Val(5), Var("z"))))
    )
  }

  it should "work in assignments" in {
    parse("let x = 5 + 6; x + 2") shouldBe Right(
      Prog(List(Asgn("x", Add(Val(5), Val(6)))), Add(Var("x"), Val(2)))
    )
  }

  it should "not allow for missing terms" in {
    parse("+ 5") shouldBe a[Left[_, _]]
    parse("x + y +") shouldBe a[Left[_, _]]
  }

  it should "not interleave with parentheses" in {
    parse("(4 +) 5") shouldBe a[Left[_, _]]
  }

  "term" should "parse multiplications in a left-associative way" in {
    parse("x * 5 * z") shouldBe Right(
      Prog(Nil, Mul(Mul(Var("x"), Val(5)), Var("z")))
    )
  }

  it should "parse right-associatively in the presence of parentheses" in {
    parse("x * (5 * z)") shouldBe Right(
      Prog(Nil, Mul(Var("x"), Mul(Val(5), Var("z"))))
    )
  }

  it should "work in assignments" in {
    parse("let x = 5 * 6; x * 2") shouldBe Right(
      Prog(List(Asgn("x", Mul(Val(5), Val(6)))), Mul(Var("x"), Val(2)))
    )
  }

  it should "not allow for missing terms" in {
    parse("* 5") shouldBe a[Left[_, _]]
    parse("x * y *") shouldBe a[Left[_, _]]
  }

  it should "not interleave with parentheses" in {
    parse("(4 *) 5") shouldBe a[Left[_, _]]
  }
}
