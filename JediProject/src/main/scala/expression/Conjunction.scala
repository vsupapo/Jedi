package expression

import context._
import value._

case class Conjunction(operands: List[Expression]) extends SpecialForm {
  override def execute(env: Environment): Value = {
    var result = true

    // short-circuit execution
    // execute operands left to right until answer is known
    for(exp <- operands if result)
      val resultBoole = exp.execute(env)
      if(!resultBoole.isInstanceOf[Boole]) throw new TypeException("Arguments to && must be of type Boole")
      result = resultBoole.asInstanceOf[Boole].value && result
    
    Boole(result)
  }
}
