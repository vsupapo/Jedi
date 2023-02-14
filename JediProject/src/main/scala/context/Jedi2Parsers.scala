package context

import scala.util.parsing.combinator._
import expression._
import value._

class Jedi2Parsers extends Jedi1Parsers {

  // params parser
  // a parameter list is zero or more comma-separated identifiers bracketed by parentheses:
  // params ::= "(" ~ (identifier ~ ("," ~ identifier)*)? ~ ")"
  def params: Parser[List[Identifier]] = "(" ~> opt(identifier ~ rep("," ~> identifier)) <~ ")" ^^ {
    case Some(first ~ Nil) => List(first)
    case Some(first ~ rest) => first :: rest
    case None => List()
  }

  // lambda parser
  // lambda ::= "lambda" ~ params ~ expression
  def lambda: Parser[Lambda] = "lambda" ~> params ~ expression ^^ {
    case params ~ env => Lambda(params, env)
  }

  // block parser
  // a block is one or more semi-colon separated expressions bracketed by curly braces:
  // block ::= "{" ~ expression ~ (";" ~ expression)* ~ "}"
  def block: Parser[Block] = "{" ~> expression ~ rep(";" ~> expression) <~ "}" ^^ {
    case first ~ Nil => Block(List(first))
    case first ~ rest => Block(first :: rest)
  }

  def freeze: Parser[MakeThunk] = "freeze" ~> "(" ~> expression <~ ")" ^^ {
    case env => MakeThunk(env)
  }

  def delay: Parser[MakeText] = "delay" ~> "(" ~> expression <~ ")" ^^ {
    case env => MakeText(env)
  }

  // override of term parser
  override def term: Parser[Expression]  = lambda | freeze | delay | funCall | block | literal | "("~>expression<~")"
}


