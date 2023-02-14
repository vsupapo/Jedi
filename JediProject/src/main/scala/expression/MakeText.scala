package expression

import context._
import value._

case class MakeText(body: Expression) extends SpecialForm {
  // execute method creates and returns a text
  override def execute(env: Environment): Value = Text(body)
}
