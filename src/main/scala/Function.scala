package simplesymbols
import simplesymbols.expressions._

class Function(val name: String, val argNames: List[String], val expression: Expression) {

  def evaluate(arguments: List[Expression]): Expression = arguments.length match {
    case l if l <= argNames.length => expression.replace(argMap(arguments))
    case _ => throw new IndexOutOfBoundsException("Supplied argument count exceeds function argument count.")
  }

  private def argMap(arguments: List[Expression]): Map[String, Expression] = Map(argNames.zip(arguments):_*)
}
