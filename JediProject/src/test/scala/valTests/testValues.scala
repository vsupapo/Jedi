import value._
import context._
import expression._

@main def bulkTest() =

  val life = Exact(42)
  val two = Exact(2)
  val pi = Inexact(3.14)
  val e = Inexact(2.718)
  val twotoo = Inexact(2.0)

  println(two * life) // 84
  println(-life) // -42
  println(e * pi) // 8.53452
  println(pi * two) // 6.28
  println(two * pi) // 6.28
  println(two < life) // true
  println(e < two) // false
  println(two == twotoo) // true

  println(!Boole.TRUE || Boole.FALSE || Boole.TRUE) // true
  println(!(Boole.FALSE || Boole.TRUE) || Boole.TRUE) // true
  println(Boole.TRUE && !(Boole.FALSE && Boole.TRUE)) // true
  //println(Boole.TRUE.compare(Boole.FALSE)) // Arguments must be comparable


  try {
  println(!Boole(true) || Boole(false) || Boole(true)) // true
  println(!(Boole(false) || Boole(true)) || Boole(true)) // true
  println(Boole(true) && !(Boole(false) && Boole(true))) // true
  //println(Exact(10) + Boole(true)) // context.TypeException: Numeric operand required
  //println(Inexact(10.0) + Boole(true)) // context.TypeException: Numeric operand required
  } catch {
    case e: Exception => println(e)
  }

  val mars = Chars("Mars")
  val jupiter = Chars("Jupiter")
  val hello = Chars("Hello, ")
  val result = Chars("result = ")
  println(hello + mars) // Hello, Mars
  println(hello + jupiter) // Hello, Jupiter
  println(result + Exact(42)) // result = 42
  println(jupiter.subChars(two, Exact(5))) // pit
  println(jupiter < mars) // true
  println(Chars("Mars") == mars) // true
  println(Chars("Hello").size) // 5

  println(alu.execute(Identifier("add"), List(Exact(3), Exact(9), Inexact(2.0)))) // 14.0
  println(alu.execute(Identifier("mul"), List(Exact(3), Exact(9), Inexact(2.0)))) // 54.0
  println(alu.execute(Identifier("sub"), List(Exact(3), Exact(9), Inexact(2.0)))) // -8.0
  println(alu.execute(Identifier("div"), List(Inexact(32.0), Exact(2), Exact(2)))) // 8.0
  println(alu.execute(Identifier("add"), List(Chars("result = "), Exact(42)))) // result = 42
  println(alu.execute(Identifier("less"), List(Exact(4), Inexact(5.0)))) // true
  println(alu.execute(Identifier("less"), List(Exact(6), Inexact(5.0)))) // false
  println(alu.execute(Identifier("less"), List(Chars("cat"), Chars("bat")))) // false
  println("testing not, more, equals, unequals:")
  println(alu.execute(Identifier("not"), List(Boole.TRUE))) // false
  println(alu.execute(Identifier("not"), List(Boole(false)))) // true
  println(alu.execute(Identifier("more"), List(Exact(4), Exact(5)))) // false
  println(alu.execute(Identifier("more"), List(Exact(6), Exact(5)))) // true
  println(alu.execute(Identifier("equals"), List(Exact(6), Exact(6)))) // true
  println(alu.execute(Identifier("equals"), List(Chars("hi"), Chars("hi")))) // true
  println(alu.execute(Identifier("equals"), List(Exact(7), Inexact(6)))) // false
  println(alu.execute(Identifier("equals"), List(Chars("hi"), Chars("bye")))) // false
  println(alu.execute(Identifier("unequals"), List(Exact(6), Exact(6)))) // false
  println(alu.execute(Identifier("unequals"), List(Chars("hi"), Chars("hi")))) // false
  println(alu.execute(Identifier("unequals"), List(Exact(7), Inexact(6.0)))) // true
  println(alu.execute(Identifier("unequals"), List(Chars("hi"), Chars("bye")))) // true

  val a1 = Exact(3)
  val a2 = Inexact(2.5)
  println(a1 * a2) // 7.5

  val a3 = -Exact(67)
  println(a3) // -67

  val a4 = Exact(5)
  println(a4 / a1) // 1.6666666666666667

  val a5 = Inexact(5.0)
  println(a5 / a1) // 1.6666666666666667

  val a6 = Chars("bat")
  val a7 = Chars("man")
  println(a6 + a7) // batman

  val a8 = Chars("2 + 2 = ")
  val a9 = Exact(4)
  println(a8 + a9) // 2 + 2 = 4




