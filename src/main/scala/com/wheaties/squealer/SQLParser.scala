package com.wheaties.squealer

import com.codecommit.gll._

//TODO: move these to another file
trait Expr
case object Wildcard extends Expr
case class Term(term: String, alias: Option[String]) extends Expr
case class Count(term: String, alias: Option[String]) extends Expr
case class Select(terms: List[Expr]) extends Expr
case class From(clauses: List[Expr]) extends Expr
case class Join(term: Expr, cond: Expr) extends Expr
case class LeftJoin(term: Expr, cond: Expr) extends Expr
case class RightJoin(term: Expr, cond: Expr) extends Expr
case class FullJoin(term: Expr, cond: Expr) extends Expr
case class Where(clauses: List[Expr]) extends Expr
case object EmptyWhere extends Expr
case class Conditional(left: String, right: String) extends Expr
case class InBetween(arg: String, items: List[String]) extends Expr
case object Like extends Expr
case class SQL(selectClause: Select, fromClause: Expr, whereClause: Expr) extends Expr

object SQLParser extends Parsers with RegexParsers{

  //TODO: move these closer to where they're used. Code Complete and all that...
  val SELECT = """(?i)select""".r
  val AS = """(?i)as""".r
  val FROM = """(?i)from""".r
  val WHERE = """(?i)where""".r
  val ITEM = """[^\s,()]+""".r
  val SEP = """\s*,\s*""".r
  val BETWEEN = """(?i)between""".r
  val LIKE = """(?i)like""".r
  val STRING = """'.*'""".r
  val IN = """(?i)in""".r
  val LEFT_PAREN = """\(""".r
  val RIGHT_PAREN = """\)""".r
  val EQUATION = """[=<>]+""".r


  def statement = select ~ from ~ where ^^{
    (selectClause, fromClause, whereClause) => SQL(selectClause, fromClause, whereClause)
  }

  //TODO: nested expressions
  def where ={
    WHERE ~ chainExpression ^^{ (_, expr) => Where(expr) } |
    "" ^^{ _ => EmptyWhere }
  }

  val AND = """(?i)and""".r
  val OR = """(?i)or""".r
  def chainExpression:Parser[List[Expr]] ={
    expression ^^{ e => List(e) } |
    expression ~ AND ~ chainExpression ^^{ (e, _, listOfe) => e :: listOfe } |
    expression ~ OR ~ chainExpression ^^{ (e, _, listOfe) => e :: listOfe }
  }

  //TODO: IS NULL, IS NOT NULL
  def expression = equation | like | between | in

  //TODO: nested expressions
  def equation = ITEM ~ EQUATION ~ ITEM ^^{ (left, _, right) => Conditional(left, right) }

  //TODO: nested expressions
  def like = ITEM ~ LIKE ~ STRING ^^{ (_, _, _) => Like }

  def between =  ITEM ~ BETWEEN ~ values ^^{ (item, _, vals) => InBetween(item, vals) }

  //TODO: nested expressions
  def in = ITEM ~ IN ~ values ^^{ (item, _, vals) => InBetween(item, vals) }

  def values = LEFT_PAREN ~> chained <~ RIGHT_PAREN ^^{ (chain) => chain }

  def chained:Parser[List[String]] ={
    ITEM ~ SEP ~ chained ^^{ (left, _, more) => left :: more } |
    ITEM ^^{ (item) => List(item) }
  }

  //TODO: nested expressions, joins
  def from ={
    FROM ~ aliased ^^{ (_, table) => From(List(table)) } |
    FROM ~ aliased ~ rep(join) ^^{ (_, table, listOfJoin) => From(table :: listOfJoin) }
  }

  val JOIN = """(?i)join""".r
  val INNER = """(?i)inner""".r
  val LEFT = """(?i)left""".r
  val RIGHT = """(?i)right""".r
  val FULL = """(?i)full""".r
  val ON = """(?i)on""".r
  def join ={
    JOIN ~ aliased ~ ON ~ equation ^^{ (_, item, _, cond) => Join(item, cond) } |
    INNER ~ JOIN ~ aliased ~ ON ~ equation ^^{ (_, _, item, _, cond) => Join(item, cond) } |
    LEFT ~ JOIN ~ aliased ~ ON ~ equation ^^{ (_, _, item, _, cond) => LeftJoin(item, cond) } |
    RIGHT ~ JOIN ~ aliased ~ ON ~ equation ^^{ (_, _, item, _, cond) => RightJoin(item, cond) } |
    FULL ~ JOIN ~ aliased ~ ON ~ equation ^^{ (_, _, item, _, cond) => FullJoin(item, cond) }
  }

  def select = SELECT ~ terms ^^{ (_, t) => Select(t) }

  //TODO: remove wildcard ambiguity
  val WILDCARD = """\*""".r
  def terms:Parser[List[Expr]] ={
    WILDCARD ^^{_ => List(Wildcard) } |
    distinct ^^{ d => List(d) } |
    distinct ~ AS ~ ITEM ^^{ (dist, _, name) => List(dist.copy(alias = Some(name))) }
    distinct ~ AS ~ ITEM ~ SEP ~ terms ^^{ (dist, _, name, _, t) => dist.copy(alias = Some(name)) :: t } |
    count ^^{ c => List(c) } |
    count ~ AS ~ ITEM ~ SEP ~ terms ^^{ (cnt, _, name, _, t) => cnt.copy(alias = Some(name)) :: t } |
    aliased ^^{v => List(v)} |
    aliased ~ SEP ~ terms ^^{ (v, _, t) => v :: t }
  }

  val DISTINCT = """(?i)distinct""".r
  def distinct = DISTINCT ~ LEFT_PAREN ~> ITEM <~ RIGHT_PAREN ^^{ item => Term(item, None) }

  val COUNT = """(?i)count""".r
  def count = COUNT ~ LEFT_PAREN ~> ITEM <~ RIGHT_PAREN ^^{ item => Count(item, None) }

  def aliased ={
    ITEM ~ AS ~ ITEM ^^{ (term1, _, alias1) => Term(term1, Some(alias1)) } |
    ITEM ^^{ term1 => Term(term1, None) }
  }
}