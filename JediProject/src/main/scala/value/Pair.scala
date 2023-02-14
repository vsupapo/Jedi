package value

import context._
import expression._

case class Pair(first: Value, second: Value) extends Value {
  // Comparisons
  def compare(other: Value): Int =
    other match
      case _ => throw new TypeException("Arguments must be comparable")

  override def equals(other: Any): Boolean =
    other match
      case x: Pair => x.isInstanceOf[Pair] && x.first == this.first && x.second == this.second
      case _ => false

  // toString
  override def toString = "(" + this.first.toString + ", " + this.second.toString + ")"

  // Hashcode
  override def hashCode = this.toString.##

  // *, -, equals, toString, hashCode
}