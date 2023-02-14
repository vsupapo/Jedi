package value

import context._
import expression._

// a text is a thunk without the defining environment
case class Text(body: Expression) extends Value {
  def apply(env: Environment): Value = {
    body.execute(env)
  }
}