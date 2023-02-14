package value
import expression._
import context._

class NoneValue extends Literal with OptionValue{
  override def execute(env: Environment) = {
    Notification("None")
  }
}
