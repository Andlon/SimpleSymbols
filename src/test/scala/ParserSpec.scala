package simplesymbols
package test

import org.scalatest._
import simplesymbols.Parser._

class ParserSpec extends FlatSpec with Matchers {
  private val env = new Environment
  private val tol = 1.0e-6

  "A unary constant" should "parse correctly" in {
    val expr = "3.0"
    val func = parse(expr)

    func.eval(env) should be (3.0 +- tol)
  }

  "A binary sum" should "parse correctly" in {
    val expr = "5.0 + 2.0"
    val func = parse(expr)

    func.eval(env) should be (7.0 +- tol)
  }

  "A binary product" should "parse correctly" in {
    val expr = "5.0 * 2.0"
    val func = parse(expr)

    func.eval(env) should be (10.0 +- tol)
  }

  "A three-part sum" should "parse correctly" in {
    val expr = "5.0 + 2.0 + 3.0"
    val func = parse(expr)

    func.eval(env) should be (10.0 +- tol)
  }

  "A three-part product" should "parse correctly" in {
    val expr = "5.0 * 2.0 * 3.0"
    val func = parse(expr)

    func.eval(env) should be (30.0 +- tol)
  }
}
