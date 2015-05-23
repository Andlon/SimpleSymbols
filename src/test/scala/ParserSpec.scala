package simplesymbols
package test

import org.scalatest._
import simplesymbols.Parser._

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
}
