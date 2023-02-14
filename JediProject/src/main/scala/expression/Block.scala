package expression

import context._
import value._

case class Block(exps: List[Expression]) extends SpecialForm {
  override def execute(env: Environment): Value = {
    // identifiers declared in block are private
    val tempEnv = new Environment(env)
    val results: List[Value] = exps.map(exp => exp.execute(tempEnv))
    // value of block is value of last expression in block
    results.last
  }
}
