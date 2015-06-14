package simplesymbols
package test

import org.scalatest._
import simplesymbols.Parser._
import simplesymbols.expressions._
import simplesymbols.tokens._

class ParserSpec extends FlatSpec with Matchers {
  private val env = Map(Variable("x") -> Constant(3.0), Variable("y") -> Constant(5.0), Variable("z") -> Constant(7.0))
  private val tol = 1.0e-6

  // TODO: Find a good way to reimplement these tests. Possibly after implementing a 'simplify()' method
  "Parser.parse(expr)" should "handle unary constants" in {
    parse("3.1").eval(env).simplified should be (Constant(3.1))
  }

  it should "handle unary variables " in {
    parse("x").eval(env).simplified should be (Constant(3.0))
  }

  it should "handle unary negated variables" in {
    parse("-x").eval(env).simplified should be (Constant(-3.0))
  }

  it should "handle binary sums of constants" in {
    parse("5.0 + 2.0").eval(env).simplified should be (Constant(7.0))
  }
//
//  "A binary sum" should "parse correctly with constants" in {
//    val expr = "5.0 + 2.0"
//    val func = parse(expr)
//
//    func.eval(env) should be (7.0 +- tol)
//  }
//
//  it should "parse correctly with variables" in {
//    val expr = "5.0 + x"
//    val func = parse(expr)
//
//    func.eval(env) should be (8.0 +- tol)
//  }
//
//  "A binary product" should "parse correctly with constants" in {
//    val expr = "5.0 * 2.0"
//    val func = parse(expr)
//
//    func.eval(env) should be (10.0 +- tol)
//  }
//
//  it should "parse correctly with variables" in {
//    val expr = "5.0 * x"
//    val func = parse(expr)
//
//    func.eval(env) should be (15.0 +- tol)
//  }
//
//  "A three-part sum" should "parse correctly with constants" in {
//    val expr = "5.0 + 2.0 + 3.0"
//    val func = parse(expr)
//
//    func.eval(env) should be (10.0 +- tol)
//  }
//
//  it should "parse correctly with variables" in {
//    val expr = "x + y + z"
//    val func = parse(expr)
//
//    func.eval(env) should be (15.0 +- tol)
//  }
//
//  "A three-part product" should "parse correctly with constants" in {
//    val expr = "5.0 * 2.0 * 3.0"
//    val func = parse(expr)
//
//    func.eval(env) should be (30.0 +- tol)
//  }
//
//  it should "parse correctly with variables" in {
//    val expr = "x * y * z"
//    val func = parse(expr)
//
//    func.eval(env) should be (105.0 +- tol)
//  }
//
//  "A composite sum-product" should "parse correctly with constants" in {
//    val expr = "5.0 * 2.0 + 3.0"
//    val func = parse(expr)
//
//    func.eval(env) should be (13.0 +- tol)
//  }
//
//  it should "parse correctly with constants when reordered" in {
//    val expr = "3.0 + 5.0 * 2.0"
//    val func = parse(expr)
//
//    func.eval(env) should be (13.0 +- tol)
//  }

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
    import Tokens._
    tokenize("5.0") should equal (number(5.0) :: Nil)
    tokenize("var") should equal (variable("var") :: Nil)
    tokenize("+") should equal (plus :: Nil)
    tokenize("*") should equal (multiplication :: Nil)
    tokenize("^") should equal (exponentiation :: Nil)
    tokenize("/") should equal (division :: Nil)
    tokenize("-") should equal (minus :: Nil)
    tokenize("5") should equal (number(5) :: Nil)
    tokenize("=") should equal (equality :: Nil)
    tokenize(":=") should equal (definition :: Nil)
  }

  it should "correctly tokenize multi-token expressions" in {
    import Tokens._
    tokenize("5.0 + x") should equal (number(5.0) :: plus :: variable("x") :: Nil)
    tokenize("5.0 * y") should equal (number(5.0) :: multiplication :: variable("y") :: Nil)
    tokenize("5.0 ^ x") should equal (number(5.0) :: exponentiation :: variable("x") :: Nil)
    tokenize("x := 5.0") should equal (variable("x") :: definition :: number(5.0) :: Nil)
    tokenize("x = 5.0") should equal (variable("x") :: equality :: number(5.0) :: Nil)
    tokenize("f := 1.2 / x_2 + y - 5 * 6 ^ z = a") should equal (
      variable("f") :: definition :: number(1.2) :: division :: variable("x_2") :: plus :: variable("y") :: minus ::
        number(5) :: multiplication :: number(6) :: exponentiation :: variable("z") :: equality :: variable("a") :: Nil
    )
  }

  "Assemble" should "correctly handle single constants" in {
    import Tokens._
    assemble(number(5.0) :: Nil) should equal (Constant(5.0))
  }

  it should "correctly handle single variables" in {
    import Tokens._
    assemble(variable("x") :: Nil) should equal(Variable("x"))
  }

  it should "correctly handle binary expressions" in {
    import Tokens._
    assemble(number(5.0) :: plus :: number(1.0) :: Nil) should equal (Sum(Constant(5.0), Constant(1.0)))
    assemble(variable("x") :: plus :: variable("y") :: Nil) should equal (Sum(Variable("x"), Variable("y")))
    assemble(number(5.0) :: multiplication :: number(1.0) :: Nil) should equal (Product(Constant(5.0), Constant(1.0)))
    assemble(variable("x") :: minus :: number(1.0) :: Nil) should equal (
      Sum(Variable("x"), Product(Constant(-1.0), Constant(1.0)))
    )
  }

  it should "handle precedence in infix expressions" in {
    import Tokens._
    assemble(number(5.0) :: plus :: number(1.0) :: multiplication :: number(2.0) :: Nil) should equal (
      Sum(Constant(5.0), Product(Constant(1.0), Constant(2.0)))
    )
    assemble(number(5.0) :: multiplication :: number(1.0) :: plus :: number(2.0) :: Nil) should equal {
      Sum(Product(Constant(5.0), Constant(1.0)), Constant(2.0))
    }
  }
}
