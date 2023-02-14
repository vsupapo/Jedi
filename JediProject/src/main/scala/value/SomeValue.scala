package value
import context.TypeException

case class SomeValue(content: Value) extends Value with OptionValue{
  // Comparisons
  def compare(other: Value): Int =
    other match
      case _ => throw new TypeException("Arguments must be comparable")

  override def equals(other: Any): Boolean =
    other match
      case x: SomeValue => x.isInstanceOf[SomeValue] && this.content == x.content
      case _ => false

  // toString
  override def toString = "Some(" + this.content + ")"

  // Hashcode
  override def hashCode = this.toString.##

  // *, -, equals, toString, hashCode
}
