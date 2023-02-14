package expression

import context.Environment
import value._

case class Identifier (name: String) extends Expression{
  override def toString = name
  override def execute(env: Environment): Value = {
    val result = env.apply(this)
    // if bound to thunk or text, then it must first be thawed by calling apply method
    result match {
      case thunk: Thunk => thunk.apply() // no defining environment
      case text: Text => text.apply(env) // defining environment extracted
      case _ => result
    }
  }
}
