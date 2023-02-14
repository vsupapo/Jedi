package expression

import context._
import value._

case class Lambda(params: List[Identifier], body: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    new Closure(params, body, env) // where closure's defining environment comes from
  }
}
