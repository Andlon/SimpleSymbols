package simplesymbols
package test

import org.scalatest._
import simplesymbols.Parser._
import simplesymbols.tokens.{LeftAssocOperatorToken, NumberToken, RightAssocOperatorToken, VariableToken}

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

    names.map(isValidVariableName(_)) should equal (Seq(true, true, true, true))
  }

  "Invalid variable names" should "fail validity check" in {
    val names = Seq("0", "2a", "_a", "-c", "")

    names.map(isValidVariableName(_)) should equal (Seq(false, false, false, false, false))
  }

  "Valid numbers" should "pass validity check" in {
    val numbers = Seq("0", "100", "1.0", "9123", "123.123", ".1")

    numbers.map(isValidNumber(_)) should equal (Seq(true, true, true, true, true, true))
  }

  "Invalid numbers" should "fail validity check" in {
    val numbers = Seq(".", "", "a", "0a", "a2")

    numbers.map(isValidNumber(_)) should equal (Seq(false, false, false, false, false))
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

    tokenize(input0) should equal (Seq(NumberToken(5.0)))
    tokenize(input1) should equal (Seq(VariableToken("myvar")))
    tokenize(input2) should equal (Seq(LeftAssocOperatorToken('+', 2)))
    tokenize(input3) should equal (Seq(LeftAssocOperatorToken('*', 3)))
    tokenize(input4) should equal (Seq(RightAssocOperatorToken('^', 4)))
    tokenize(input5) should equal (Seq(LeftAssocOperatorToken('/', 3)))
    tokenize(input6) should equal (Seq(LeftAssocOperatorToken('-', 2)))
    tokenize(input7) should equal (Seq(NumberToken(5)))
  }

  it should "correctly tokenize multi-token expressions" in {
    tokenize("5.0 + x") should equal (Seq(NumberToken(5.0), LeftAssocOperatorToken('+', 2), VariableToken("x")))
    tokenize("5.0 * y") should equal (Seq(NumberToken(5.0), LeftAssocOperatorToken('*', 3), VariableToken("y")))
    tokenize("5.0 ^ x") should equal (Seq(NumberToken(5.0), RightAssocOperatorToken('^', 4), VariableToken("x")))
    tokenize("1.2 / x_2 + y - 5 * 6 ^ z") should equal (Seq(
      NumberToken(1.2),
      LeftAssocOperatorToken('/', 3),
      VariableToken("x_2"),
      LeftAssocOperatorToken('+', 2),
      VariableToken("y"),
      LeftAssocOperatorToken('-', 2),
      NumberToken(5),
      LeftAssocOperatorToken('*', 3),
      NumberToken(6),
      RightAssocOperatorToken('^', 4),
      VariableToken("z")
    ))
  }
}
