package value

import context.{TypeException, IllegalValueException}

case class Inexact(value: Double) extends Numeric with Ordered[Value] :

  // Operator overloading
  def +(other: Value): Addable =
    other match
      case x: Exact => Inexact(this.value + x.value.toDouble)
      case x: Inexact => Inexact(this.value + x.value)
      case _ => throw new TypeException("Numeric operand required")

  def *(other: Value): Numeric =
    other match {
      case x: Exact => Inexact(this.value * x.value.toDouble)
      case x: Inexact => Inexact(this.value * x.value)
      case _ => throw new TypeException("Numeric operand required")
    }

  def -(other: Value): Numeric =
    other match
      case x: Exact => Inexact(this.value - x.value.toDouble)
      case x: Inexact => Inexact(this.value - x.value)
      case _ => throw new TypeException("Numeric operand required")

  def /(other: Value): Numeric =
    other match
      case x: Exact =>
        if (x.value == 0) throw IllegalValueException("Can't divide by 0")
        else Inexact(this.value / x.value.toDouble)
      case x: Inexact =>
        if (x.value == 0.0) throw IllegalValueException("Can't divide by 0")
        else Inexact(this.value / x.value)
      case _ => throw new TypeException("Numeric operand required")

  def unary_- = Inexact(-this.value)

  // Comparisons
  def compare(other: Value): Int =
    other match
      case x: Exact => this.value.compare(x.value.toDouble)
      case x: Inexact => this.value.compare(x.value)
      case _ => throw new TypeException("Arguments must be comparable")

  override def equals(other: Any): Boolean =
    other match
      case x: Inexact => x.isInstanceOf[Inexact] && x.value == this.value
      case x: Exact => x.isInstanceOf[Exact] && x.value.toDouble == this.value
      case _ => false

  // toString
  override def toString = this.value.toString

  // Hashcode
  override def hashCode = this.toString.##

// *, -, equals, toString, hashCode
