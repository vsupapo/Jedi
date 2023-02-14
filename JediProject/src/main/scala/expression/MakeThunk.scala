package expression

import context._
import value._

case class MakeThunk(body: Expression) extends SpecialForm {
  // execute method creates and returns a thunk
  override def execute(env: Environment): Value = Thunk(body, env)
}
