package expression
import context._
import value._

case class Iteration(condition: Expression, body: Expression) extends SpecialForm {
  override def execute(env: Environment): Value =
    var cond = condition.execute(env)
    var break = false

    if (!cond.isInstanceOf[Boole]) throw new TypeException("While loops condition must be a Boole")
    // while condition is true, try to execute body expression
    while(!break && cond.isInstanceOf[Boole] && cond.asInstanceOf[Boole].value)
      try
        body.execute(env)
        cond = condition.execute(env)
      catch
        case e: UndefinedException => println(e.getMessage); break = true
    Notification.DONE
}