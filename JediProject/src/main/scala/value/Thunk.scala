package value

import context._
import expression._

// a thunk is essentially a parameterless closure
case class Thunk(body: Expression, defEnv: Environment) extends Closure(Nil, body, defEnv) {
  // initiate to default value
  var result: Value = null

  // thunk calls apply method from closure but with no arguments
  // cache result of apply on first call in order to prevent doing redundant work
  // prevents having to execute body of thunk multiple times
  def apply(): Value = {
    if(result == null) result = super.apply(Nil) 
    result
  }
}

