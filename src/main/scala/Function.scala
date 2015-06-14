package simplesymbols
import simplesymbols.expressions._

class Function(val name: String, val args: List[Variable], val expression: Expression) {

  def evaluate(arguments: List[Option[Expression]]): Expression = arguments.length match {
    case l if l <= args.length => expression.eval(argMap(arguments))
    case _ => throw new IndexOutOfBoundsException("Supplied argument count exceeds function argument count.")
  }

  private def argMap(arguments: List[Option[Expression]]): Map[Variable, Expression] =
    Map(args.zip(arguments):_*).collect({ case (k, Some(v)) => (k, v)})
}
