package simplesymbols

import simplesymbols.functions._

object Parser {
  def parse(expression: String): Function = parseForSum(expression.filterNot(_.isSpaceChar))

  private def parseForSum(expression: String): Function = {
    val (leftExpr, rightExpr) = splitBySymbol(expression, '+')
    val leftFunc = parseForProduct(leftExpr)
    lazy val rightFunc = parseForSum(rightExpr)

    rightExpr match {
      case "" => leftFunc
      case _ => new Sum(leftFunc, rightFunc)
    }
  }

  private def parseForProduct(expression: String): Function = {
    val (leftExpr, rightExpr) = splitBySymbol(expression, '*')

    // If the left expression is purely numeric, it is a constant, otherwise it is a variable
    lazy val rightFunc = parseForProduct(rightExpr)
    val leftFunc = {
      try { new Constant(leftExpr.toDouble) }
      catch { case _: NumberFormatException => new Variable(leftExpr) }
    }

    rightExpr match {
      case "" => leftFunc
      case _ => new Product(leftFunc, rightFunc)
    }
  }

  private def splitBySymbol(str: String, symbol: Char) = str.indexOf(symbol) match {
    case -1 => (str, "")
    case n => (str take n, str drop n + 1)
  }
}
