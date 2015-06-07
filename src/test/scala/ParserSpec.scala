package simplesymbols
package test

import org.scalatest._
import simplesymbols.Parser._
import simplesymbols.expressions._
import simplesymbols.tokens._

class ParserSpec extends FlatSpec with Matchers {
  private val env = new Environment(Map("x" -> 3.0, "y" -> 5.0, "z" -> 7.0))
  private val tol = 1.0e-6

  "A unary constant" should "parse correctly" in {
    val expr = "3.0"
    val func = parse(expr)

    func.eval(env) should be (3.0 +- tol)
  }

  "A unary variable" should "parse correctly" in {
    val expr = "x"
    val func = parse(expr)

    func.eval(env) should be (3.0 +- tol)
  }

  "A unary negated variable" should "parse correctly" in {
    val expr = "-x"
    val func = parse(expr)

    func.eval(env) should be (-3.0 +- tol)
  }

  "A binary sum" should "parse correctly with constants" in {
    val expr = "5.0 + 2.0"
    val func = parse(expr)

    func.eval(env) should be (7.0 +- tol)
  }

  it should "parse correctly with variables" in {
    val expr = "5.0 + x"
    val func = parse(expr)

    func.eval(env) should be (8.0 +- tol)
  }

  "A binary product" should "parse correctly with constants" in {
    val expr = "5.0 * 2.0"
    val func = parse(expr)

    func.eval(env) should be (10.0 +- tol)
  }

  it should "parse correctly with variables" in {
    val expr = "5.0 * x"
    val func = parse(expr)

    func.eval(env) should be (15.0 +- tol)
  }

  "A three-part sum" should "parse correctly with constants" in {
    val expr = "5.0 + 2.0 + 3.0"
    val func = parse(expr)

    func.eval(env) should be (10.0 +- tol)
  }

  it should "parse correctly with variables" in {
    val expr = "x + y + z"
    val func = parse(expr)

    func.eval(env) should be (15.0 +- tol)
  }

  "A three-part product" should "parse correctly with constants" in {
    val expr = "5.0 * 2.0 * 3.0"
    val func = parse(expr)

    func.eval(env) should be (30.0 +- tol)
  }

  it should "parse correctly with variables" in {
    val expr = "x * y * z"
    val func = parse(expr)

    func.eval(env) should be (105.0 +- tol)
  }

  "A composite sum-product" should "parse correctly with constants" in {
    val expr = "5.0 * 2.0 + 3.0"
    val func = parse(expr)

    func.eval(env) should be (13.0 +- tol)
  }

  it should "parse correctly with constants when reordered" in {
    val expr = "3.0 + 5.0 * 2.0"
    val func = parse(expr)

    func.eval(env) should be (13.0 +- tol)
  }

  "Valid variable names" should "pass validity check" in {
    val names = Seq("a", "bc", "a2", "a_2")

    names.map(isVariableName(_)) should equal (Seq(true, true, true, true))
  }

  "Invalid variable names" should "fail validity check" in {
    val names = Seq("0", "2a", "_a", "-c", "")

    names.map(isVariableName(_)) should equal (Seq(false, false, false, false, false))
  }

  "Valid numbers" should "pass validity check" in {
    val numbers = Seq("0", "100", "1.0", "9123", "123.123", ".1")

    numbers.map(isNumber(_)) should equal (Seq(true, true, true, true, true, true))
  }

  "Invalid numbers" should "fail validity check" in {
    val numbers = Seq(".", "", "a", "0a", "a2")

    numbers.map(isNumber(_)) should equal (Seq(false, false, false, false, false))
  }

  "Tokenize" should "correctly tokenize single-token expressions" in {
    val input0 = "5.0"
    val input1 = "myvar"
    val input2 = "+"
    val input3 = "*"
    val input4 = "^"
    val input5 = "/"
    val input6 = "-"
    val input7 = "5"

    import Tokens._
    tokenize(input0) should equal (Seq(NumberToken(5.0)))
    tokenize(input1) should equal (Seq(VariableToken("myvar")))
    tokenize(input2) should equal (Seq(Tokens.plus))
    tokenize(input3) should equal (Seq(Tokens.multiplication))
    tokenize(input4) should equal (Seq(Tokens.exponentiation))
    tokenize(input5) should equal (Seq(Tokens.division))
    tokenize(input6) should equal (Seq(Tokens.minus))
    tokenize(input7) should equal (Seq(NumberToken(5)))
    tokenize("=") should equal (equality :: Nil)
    tokenize(":=") should equal (definition :: Nil)
  }

  it should "correctly tokenize multi-token expressions" in {
    import Tokens._
    tokenize("5.0 + x") should equal (Seq(NumberToken(5.0), plus, VariableToken("x")))
    tokenize("5.0 * y") should equal (Seq(NumberToken(5.0), multiplication, VariableToken("y")))
    tokenize("5.0 ^ x") should equal (Seq(NumberToken(5.0), exponentiation, VariableToken("x")))
    tokenize("1.2 / x_2 + y - 5 * 6 ^ z") should equal (Seq(
      NumberToken(1.2),
      division,
      VariableToken("x_2"),
      plus,
      VariableToken("y"),
      minus,
      NumberToken(5),
      multiplication,
      NumberToken(6),
      exponentiation,
      VariableToken("z")
    ))

    tokenize("x := 5.0") should equal (variable("x") :: definition :: number(5.0) :: Nil)
    tokenize("x = 5.0") should equal (variable("x") :: equality :: number(5.0) :: Nil)
  }

  "Assemble" should "correctly handle single constants" in {
    assemble(Seq(NumberToken(5.0))) should equal (new Constant(5.0))
  }

  it should "correctly handle single variables" in {
    assemble(Seq(VariableToken("x"))) should equal(new Variable("x"))
  }

  it should "correctly handle binary expressions" in {
    import Tokens._
    assemble(Seq(NumberToken(5.0), plus, NumberToken(1.0))) should equal (Sum(Constant(5.0), Constant(1.0)))
    assemble(Seq(VariableToken("x"), plus, VariableToken("y"))) should equal (Sum(Variable("x"), Variable("y")))
    assemble(Seq(NumberToken(5.0), multiplication, NumberToken(1.0))) should equal (Product(Constant(5.0), Constant(1.0)))
    assemble(Seq(VariableToken("x"), minus, NumberToken(1.0))) should equal (
      Sum(Variable("x"), Product(Constant(-1.0), Constant(1.0)))
    )
  }

  it should "handle precedence in infix expressions" in {
    import Tokens._
    assemble(Seq(NumberToken(5.0), plus, NumberToken(1.0), multiplication, NumberToken(2.0))) should equal (
      Sum(Constant(5.0), Product(Constant(1.0), Constant(2.0)))
    )
    assemble(Seq(NumberToken(5.0), multiplication, NumberToken(1.0), plus, NumberToken(2.0))) should equal {
      Sum(Product(Constant(5.0), Constant(1.0)), Constant(2.0))
    }
  }
}
