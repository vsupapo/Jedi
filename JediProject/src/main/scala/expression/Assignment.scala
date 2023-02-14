package expression

import context.{Environment, TypeException}
import value.{Notification, Variable}

case class Assignment(vbl: Identifier, update: Expression) extends SpecialForm {
  def execute(env: Environment) =
    val v = vbl.execute(env)
    v match
      case x: Variable => x.content = update.execute(env)
        Notification.DONE
      case _ => throw new TypeException("Input to assignment must be a variable")
}