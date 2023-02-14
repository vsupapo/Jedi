package value
import context._
import expression._

class Closure(params: List[Identifier], body: Expression, defEnv: Environment) extends Value {
  def apply(args: List[Value]): Value = {
    if(args.length != params.length) throw new JediException("Wrong number of arguments") // ensure lists have same size
    val localEnv = new Environment(defEnv) // functions remember their defining environment
    localEnv.bulkPut(params, args) // binding parameters to its arguments
    body.execute(localEnv)
  }

  def apply(args:List[Value], callEnv: Environment): Value = {
    if(args.length != params.length) throw new JediException("Wrong number of arguments") // ensure lists have same size
    val localEnv = new Environment(callEnv) // functions remember their defining environment
    localEnv.bulkPut(params, args) // binding parameters to its arguments
    body.execute(localEnv)
  }
}