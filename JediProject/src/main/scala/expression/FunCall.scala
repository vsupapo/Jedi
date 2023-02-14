package expression

import context._
import value._

case class FunCall(operator: Identifier, operands: List[Expression]) extends Expression:
  override def execute(env: Environment): Value = {
    var args: List[Value] = Nil
    if (env.contains(operator))
      if (flags.paramPassing == flags.BY_NAME)
        val thunk = env.apply(operator).asInstanceOf[Closure]
        // turn operands into thunks
        args = operands.map(operands => MakeThunk(operands).execute(env))
        thunk.apply(args)
      else if (flags.paramPassing == flags.BY_TEXT)
        val text = env.apply(operator).asInstanceOf[Closure]
        // turn operands into texts
        args = operands.map(operands => MakeText(operands).execute(env))
        text.apply(args)
      else
        val closure = env.apply(operator).asInstanceOf[Closure]
        if (flags.staticScoping == false)
          closure.apply(operands.map(operands => operands.execute(env)), env)
        else
          closure.apply(operands.map(operands => operands.execute(env)))
    else
      // eager execution
      args = operands.map(_.execute(env))
      alu.execute(operator, args)
  }