package simplesymbols
package test

import org.scalatest._
import simplesymbols.expressions._

class FunctionSpec extends FlatSpec with Matchers {
  //private val env = new Environment(Map("x" -> 2.0, "y" -> 3.0))
  private val tol = 1.0e-6

  private val x = new Variable("x")
  private val y = new Variable("y")
  private val c = new Constant(2.0)
  private val d = new Constant(3.0)

//  "A constant" should "eval to itself" in {
//    val c = new Constant(5.0)
//    c.eval(env) should be (5.0)
//  }
//
//  "A sum of two constants" should "eval correctly" in {
//    val s = new Sum(c, d)
//    s.eval(env) should be (5.0 +- tol)
//  }
//
//  it should "commute" in {
//    val s1 = new Sum(c, d)
//    val s2 = new Sum(d, c)
//    s1.eval(env) should be (s2.eval(env) +- tol)
//  }
//
//  "A variable" should "eval to itself" in {
//    x.eval(env) should be (2.0 +- tol)
//  }
//
//  "A sum of a variable and a constant" should "eval correctly" in {
//    val s = new Sum(x, d)
//    s.eval(env) should be (5.0 +- tol)
//  }
//
//  it should "commute" in {
//    val s1 = new Sum(x, d)
//    val s2 = new Sum(d, x)
//    s1.eval(env) should be (s2.eval(env) +- tol)
//  }
//
//  "A sum of two variables" should "eval to correct sum" in {
//    val s = new Sum(x, y)
//    s.eval(env) should be (5.0 +- tol)
//  }
//
//  it should "commute" in {
//    val s1 = new Sum(x, y)
//    val s2 = new Sum(y, x)
//    s1.eval(env) should be (s2.eval(env) +- tol)
//  }
//
//  "A product of two constants" should "eval correctly" in {
//    val p = new Product(c, d)
//    p.eval(env) should be (6.0 +- tol)
//  }
//
//  it should "commute" in {
//    val p1 = new Product(c, d)
//    val p2 = new Product(d, c)
//    p1.eval(env) should be (p2.eval(env) +- tol)
//  }
//
//  "A constant times a variable" should "eval correctly" in {
//    val p = new Product(new Constant(3.0), x)
//    p.eval(env) should be (6.0 +- tol)
//  }
//
//  it should "commute" in {
//    val c = new Constant(3.0)
//    val p1 = new Product(c, x)
//    val p2 = new Product(x, c)
//    p1.eval(env) should be (p2.eval(env) +- tol)
//  }
}