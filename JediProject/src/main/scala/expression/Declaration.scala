package expression

import context._
import value._

case class Declaration(identifier: Identifier, expression: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    env.put(identifier, expression.execute(env)) // Adds new row to env map
    Notification.OK
  }
}
