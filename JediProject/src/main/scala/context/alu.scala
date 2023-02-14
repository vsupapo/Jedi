package context

import expression.Identifier
import value.*

import scala.collection.mutable.ArrayBuffer

object alu:

  def execute(opcode: Identifier, args: List[Value]): Value =
    opcode.name match
      case "add" => add(args)            // n-ary
      case "mul" => mul(args)            // n-ary
      case "sub" => sub(args)            // n-ary
      case "div" => div(args)            // n-ary
      case "less" => less(args)          // binary
      case "equals" => same(args)        // binary
      case "more" => more(args)          // binary
      case "unequals" => unequals(args)  // binary
      case "not" => not(args)            // unary
      // primitive I/O ops
      case "write" => write(args)
      // pairs and lists
      case "cons" => cons(args)
      case "car" => car(args)
      case "cdr" => cdr(args)
      case "nil" => getEmpty()
      case "list" => list(args)
      case "size" => size(args)
      case "nth" => nth(args)
      // variables
      case "dereference" => dereference(args)
      case "var" => makeVar(args)
      // store ops
      case "store" => store(args)
      case "put" => put(args)
      case "rem" => rem(args)
      case "contains" => contains(args)
      case "map" => map(args)
      case "filter" => filter(args)
      case "get" => get(args)
      case "addLast" => addLast(args)
      // final ops
      case "Some" => some(args)
      case "getVal" => getVal(args)
      case _ => throw new UndefinedException(opcode)
  // TBC

  private def add(args: List[Value]): Value =
    def helper(result: Addable, unseen: List[Value]): Addable =
      if(unseen.isEmpty) result
      else unseen.head match
        case h: Addable => helper(result + h, unseen.tail)
        case x: Variable => throw TypeException("Inputs to + must be numbers or texts")
        case _ => throw TypeException("Inputs to + must be addable")

    if(args.size < 2) throw new TypeException("2 or more inputs required by +")
    args(0) match
      case n: Addable => helper(n, args.tail )
      case x: Variable => throw TypeException("Inputs to + must be numbers or texts")
      case _ => throw new TypeException("Inputs to + must be addable")

  private def mul(args: List[Value]): Value =
    def helper(result: value.Numeric, unseen: List[Value]): value.Numeric =
      if(unseen.isEmpty) result
      else unseen.head match
        case h: value.Numeric => helper(result * h, unseen.tail)
        case x: Variable => throw TypeException("Inputs to + must be numbers or texts")
        case _ => throw TypeException("Inputs to * must be numeric")

    if(args.size < 2) throw new TypeException("2 or more inputs required by *")
    args(0) match
      case n: value.Numeric => helper(n, args.tail )
      case x: Variable => throw TypeException("Inputs to + must be numbers or texts")
      case _ => throw new TypeException("Inputs to * must be numeric")

  private def sub(args: List[Value]): Value =
    def helper(result: value.Numeric, unseen: List[Value]): value.Numeric =
      if (unseen.isEmpty) result
      else unseen.head match
        case h: value.Numeric => helper(result - h, unseen.tail)
        case x: Variable => throw TypeException("Inputs to + must be numbers or texts")
        case _ => throw TypeException("Inputs to - must be numeric")

    if (args.size < 2) throw new TypeException("2 or more inputs required by -")
    args(0) match
      case n: value.Numeric => helper(n, args.tail)
      case x: Variable => throw TypeException("Inputs to + must be numbers or texts")
      case _ => throw new TypeException("Inputs to - must be numeric")

  private def div(args: List[Value]): Value =
    def helper(result: value.Numeric, unseen: List[Value]): value.Numeric =
      if (unseen.isEmpty) result
      else unseen.head match
        case h: value.Numeric => helper(result / h, unseen.tail)
        case x: Variable => throw TypeException("Inputs to + must be numbers or texts")
        case _ => throw TypeException("Inputs to / must be numeric")

    if (args.size < 2) throw new TypeException("Inputs to / expects 2 or more inputs")
    args(0) match
      case n: value.Numeric => helper(n, args.tail)
      case x: Variable => throw TypeException("Inputs to + must be numbers or texts")
      case _ => throw new TypeException("Inputs to / must be numeric")

  private def less(args: List[Value]): Value =
    if (args.size != 2) throw new TypeException("Inputs to < require 2 inputs")
    args(0) match
      case x: Ordered[Value] => Boole(x < args(1))
      case _ => throw TypeException("Inputs to < must be orderable")

  private def same(args: List[Value]): Value =
    if (args.size != 2) throw new TypeException("Inputs to equals expects 2 inputs")
    else
      val a = args.head
      for (e <- args)
        if (a != e) return Boole(false)
      Boole(true)

  private def more(args: List[Value]): Value =
    if(args.size != 2) throw new TypeException("Inputs to > require 2 inputs")
    args(0) match
      case x: Ordered[Value] => Boole(x > args(1))
      case _ => throw TypeException("Inputs to > must be orderable")

  private def unequals(args: List[Value]): Value =
    if (args.length  != 2) throw new TypeException("Inputs to unequals require 2 inputs")
    else
      val a = args.head
      for (e <- args)
        if (a != e) return Boole(true)
      Boole(false)

  private def not(args: List[Value]): Value =
    if(args.size != 1) throw new TypeException("Inputs to not require 1 input")
    args(0) match
      case x: Boole => !x
      case _ => throw new TypeException("Inputs to not expects number or boole")

  private def write(args: List[Value]): Value =
    println(args(0))
    Notification.DONE

  private def cons(args: List[Value]): Value =
    if(args.size != 2) throw new TypeException("Inputs to cons require 2 inputs")
    else Pair(args(0), args(1))

  private def car(args: List[Value]): Value =
    if(args.size != 1) throw new TypeException("Inputs to car requires 1 input")
    args(0) match
      case x: Pair => x.first

  private def cdr(args: List[Value]): Value =
    if(args.size != 1) throw new TypeException("Inputs to car requires 1 input")
    args(0) match
      case x: Pair => x.second

  private def getEmpty(): Value =
    Chars("Nil")

  private def list(args: List[Value]): Value =
    val list = args.reverse
    var pair = getEmpty()
    for (a <- list)
      pair = Pair(a, pair)
    pair

  private def size(args: List[Value]): Value =
    if (args.size == 1 && args(0).isInstanceOf[Store])
      val value = args(0).asInstanceOf[Store].size
      Exact(value)
    else
      var result: value.Addable = Exact(0)
      var myPair = Pair(getEmpty(), getEmpty())
      var continue = true

      args(0) match
        case x: Pair => myPair = x
        case _ => throw new TypeException("Inputs to size requires cons input")

      while(myPair.isInstanceOf[Pair] && continue)
        result = result + Exact(1)
        val mySec = myPair.second
        mySec match
          case x: Pair => myPair = x
          case _ => continue = false
      result

  private def nth(args: List[Value]): Value =
    var result: Value = getEmpty()
    var myPair = Pair(getEmpty(), getEmpty())
    var index = 0
    var counter = 0

    args(0) match
      case x: Pair => myPair = x
      case _ => throw new TypeException("Inputs to nth requires cons and index input")

    args(1) match
      case x: Exact => index = x.value
      case _ => throw new TypeException("Inputs to nth requires cons and index input")

    while(myPair.isInstanceOf[Pair] && counter <= index)
      counter = counter + 1
      result = myPair.first
      val mySec = myPair.second
      mySec match
        case x: Pair => myPair = x
        case x: Value =>
          result = myPair.first
          myPair = Pair(getEmpty(), getEmpty())
    result

  private def makeVar(args: List[Value]): Value =
    if(args.size == 1) new Variable(args.head)
    else throw new TypeException("Variables can only have 1 argument")

  private def dereference(args: List[Value]): Value =
    args match
      case List(a) =>
        Some(a)
          .filter(_.isInstanceOf[Variable])
          .map(_.asInstanceOf[Variable])
          .map(_.content)
          .getOrElse(throw new TypeException("Input expects a variable"))
      case _ => throw new TypeException("Unable to dereference variable")

  private def store(args: List[Value]) =
    Store(args.to(ArrayBuffer))

  private def put(args: List[Value]) =
    if (args.size != 3 || !args(1).isInstanceOf[Exact] || !args(2).isInstanceOf[Store])
      throw new TypeException("Unexpected signature: put(v: Value, p: Integer, s: Store)")
    args(2).asInstanceOf[Store].put(args(0), args(1).asInstanceOf[Exact].value)
    Notification.DONE

  private def rem(args: List[Value]) =
    if (args.size != 2 || !args(0).isInstanceOf[Exact] || !args(1).isInstanceOf[Store])
      throw new TypeException("Unexpected signature: rem(v: Value, s: Store)")
    args(1).asInstanceOf[Store].rem(args(0).asInstanceOf[Exact].value)
    Notification.DONE

  private def get(args: List[Value]) =
    if (args.size != 2 || !args(0).isInstanceOf[Exact] || !args(1).isInstanceOf[Store])
      throw new TypeException("Unexpected signature: get(v: Value, s: Store)")
    args(1).asInstanceOf[Store].get(args(0).asInstanceOf[Exact].value)

  private def map(args: List[Value]) =
    if (args.size != 2 || !args(0).isInstanceOf[Closure] || !args(1).isInstanceOf[Store])
      throw new TypeException("Unexpected signature: map(c: Closure, s: Store)")
    args(1).asInstanceOf[Store].map(args(0).asInstanceOf[Closure])

  private def filter(args: List[Value]) =
    if (args.size != 2 || !args(0).isInstanceOf[Closure] || !args(1).isInstanceOf[Store])
      throw new TypeException("Unexpected signature: filter(c: Closure, s: Store)")
    args(1).asInstanceOf[Store].filter(args(0).asInstanceOf[Closure])

  private def contains(args: List[Value]) =
    if (args.size != 2 || !args(0).isInstanceOf[Value] || !args(1).isInstanceOf[Store])
      throw new TypeException("Unexpected signature: contains(v: Value, s: Store)")
    args(1).asInstanceOf[Store].contains(args(0))

  private def addLast(args: List[Value]) =
    if (args.size != 2 || !args(0).isInstanceOf[Value] || !args(1).isInstanceOf[Store])
      throw new TypeException("Unexpected signature: addLast(v: Value, s: Store)")
    args(1).asInstanceOf[Store].add(args(0))
    Notification.DONE

  private def some(args: List[Value]) =
    if (args.size != 1)
      throw new TypeException("Unexpected input")
    else
      SomeValue(args(0))

  private def getVal(args: List[Value]) =
    var result: Value = getEmpty()
    if (args.size != 1)
      throw new TypeException("Unexpected input")
    if (args(0).isInstanceOf[SomeValue])
      val mySome = args(0).asInstanceOf[SomeValue]
      result = mySome.v
    result

// etc.