package simplesymbols
package exceptions

object ExceptionUtils {
  def noVarMessage(variableName: String) = "No variable %s in environment".format(variableName)
}

class NoSuchVariableException(val variableName: String) extends Exception(ExceptionUtils.noVarMessage(variableName)) {}