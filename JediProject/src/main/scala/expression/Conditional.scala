package expression
import context._
import value._

case class Conditional(condition: Expression, consequent: Expression, alternative: Expression = null) extends SpecialForm {
  override def execute(env: Environment): Value = {
    val result: Value = condition.execute(env) // Execute condition
    if (!result.isInstanceOf[Boole]) return Notification.UNSPECIFIED // If the result is not an instance of Boole, throw error
    
    val resultBoole = result.asInstanceOf[Boole].value

    // If result is true, execute consequent and ignore alternative
    if (resultBoole && alternative != null) consequent.execute(env)
    else if (resultBoole && alternative == null) Notification.UNSPECIFIED
    // If result is false and alternative is not null, execute alternative and ignore consequent
    else if (alternative != null) alternative.execute(env)
    else if (alternative == null) Notification.UNSPECIFIED
    else Notification.DONE
  }
}
