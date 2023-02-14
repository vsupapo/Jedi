package value

import context.{TypeException}

case class Chars(value: String) extends Addable with Ordered[Value]{

  // Chars methods
  def subChars(to: Exact, from: Exact): Chars =
    Chars(value.substring(to.value, from.value))

  def size: Exact = Exact(value.size)

  // Operator overloading
  override def +(other: Value) =
    other match
      case x: Chars => Chars(this.value + other)
      case x: Exact => Chars(this.value + other)
      case x: Inexact => Chars(this.value + other)
      case _ => throw new TypeException("Arguments must be compatible")

  // Comparisons
  def compare(other: Value): Int =
    other match
      case x: Chars => if (this.value < x.value) -1
                      else if (x.value < this.value) 1
                      else 0
      case _ => throw new TypeException("Arguments must be comparable")

  override def equals(other: Any): Boolean =
    other match
      case other: Chars => this.canEqual(other) && (other.value == this.value)
      case _ => false

  // toString
  override def toString = value

  // Hashcode
  override def hashCode = this.toString.##
}