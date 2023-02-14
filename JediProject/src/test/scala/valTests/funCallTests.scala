package valTests

import expression._
import context._
import value._


object funCallTest extends App {
  val globalEnvironment = new Environment
  val operands = List(Exact(6), Exact(7))

  var exp = FunCall(Identifier("add"), operands)
  println(exp.execute(globalEnvironment)) // 13
  exp = FunCall(Identifier("less"), operands)
  println(exp.execute(globalEnvironment)) // true
  exp = FunCall(Identifier("mul"), operands)
  println(exp.execute(globalEnvironment)) // 42
  exp = FunCall(Identifier("sub"), operands)
  println(exp.execute(globalEnvironment)) // -1
  exp = FunCall(Identifier("div"), operands)
  println(exp.execute(globalEnvironment)) // 0.8571428571428571

  val operands1 = List(Inexact(30.0), Inexact(6.0))

  exp = FunCall(Identifier("add"), operands1)
  println(exp.execute(globalEnvironment)) // 36.0
  exp = FunCall(Identifier("less"), operands1)
  println(exp.execute(globalEnvironment)) // false
  exp = FunCall(Identifier("mul"), operands1)
  println(exp.execute(globalEnvironment)) // 180.0
  exp = FunCall(Identifier("sub"), operands1)
  println(exp.execute(globalEnvironment)) // 24.0
  exp = FunCall(Identifier("div"), operands1)
  println(exp.execute(globalEnvironment)) // 5.0

  val operands2 = List(Inexact(10.0), Exact(2))

  exp = FunCall(Identifier("add"), operands2)
  println(exp.execute(globalEnvironment)) // 12.0
  exp = FunCall(Identifier("less"), operands2)
  println(exp.execute(globalEnvironment)) // false
  exp = FunCall(Identifier("mul"), operands2)
  println(exp.execute(globalEnvironment)) // 20.0
  exp = FunCall(Identifier("sub"), operands2)
  println(exp.execute(globalEnvironment)) // 8.0
  exp = FunCall(Identifier("div"), operands2)
  println(exp.execute(globalEnvironment)) // 5.0

  val operands3 = List(Inexact(12.0), Exact(2), Exact(2))

  exp = FunCall(Identifier("add"), operands3)
  println(exp.execute(globalEnvironment)) // 16.0
  exp = FunCall(Identifier("mul"), operands3)
  println(exp.execute(globalEnvironment)) // 48.0
  exp = FunCall(Identifier("sub"), operands3)
  println(exp.execute(globalEnvironment)) // 8.0
  exp = FunCall(Identifier("div"), operands3)
  println(exp.execute(globalEnvironment)) // 3.0
}
