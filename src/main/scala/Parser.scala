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
    val leftFunc = parseUnary(leftExpr)

    rightExpr match {
      case "" => leftFunc
      case _ => new Product(leftFunc, rightFunc)
    }
  }

  private def splitBySymbol(str: String, symbol: Char) = str.indexOf(symbol) match {
    case -1 => (str, "")
    case n => (str take n, str drop n + 1)
  }

  private def parseUnary(expression: String): Function =
    try { new Constant(expression.toDouble) }
    catch { case _: NumberFormatException => {
      expression.head match {
        case '-' => new Product(new Constant(-1.0), parseUnary(expression.tail))
        case _ => new Variable(expression)
      }
    }
    }
}
