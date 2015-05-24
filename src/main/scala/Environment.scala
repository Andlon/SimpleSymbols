package simplesymbols

import simplesymbols.exceptions.NoSuchVariableException

import scala.collection.immutable.Map

class Environment(private var vars: Map[String, Double]) {
  def contains(name: String) = vars.contains(name)

  def set(name: String, value: Double): Unit = vars += name -> value

  def get(name: String) = vars.get(name) match {
    case Some(x) => x
    case None => throw new NoSuchVariableException(name)
  }

  def this() = this(Map.empty)
}
