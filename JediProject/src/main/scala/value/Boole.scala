package value

import expression.Literal
import context.{TypeException}

case class Boole(value: Boolean) extends Literal {
  
  // Operator overloading
  def &&(other: Boole) = Boole(this.value && other.value)
  def ||(other: Boole) = Boole(this.value || other.value)
  def unary_! = Boole(!this.value)

  // Comparisons
  def compare(other: Value) = throw new TypeException("Arguments must be comparable")
  
  override def equals(other: Any): Boolean =
    other match
      case other: Boole => this.canEqual(other) && (other.value == this.value)
      case _ => false

  // toString
  override def toString = if(value) "true" else "false"

  // Hashcode
  override def hashCode = this.toString.##
}

// Companion object to Boole to store static variables
object Boole {
  val TRUE = Boole(true)
  val FALSE = Boole(false)
}
