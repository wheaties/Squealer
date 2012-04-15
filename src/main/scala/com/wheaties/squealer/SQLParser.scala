package com.wheaties.squealer

import com.codecommit.gll._

object SQLParser extends Parsers with RegexParsers with (String => Stream[Result[Expr]]){

  def apply(input: String) = parser(input)

  protected[squealer] def parser = statement | union

  val UNION = """(?i)union""".r
  val ALL = """(?i)all""".r
  protected[squealer] def union:Parser[Expr] ={
    statement ~ UNION ~ statement ^^{ (left, _, right) => Union(left, right) } |
    statement ~ UNION ~ union ^^{ (left, _, right) => Union(left, right) } |
    statement ~ UNION ~ ALL ~ statement ^^{ (left, _, _, right) => UnionAll(left, right) } |
    statement ~ UNION ~ ALL ~ union ^^{ (left, _, _, right) => UnionAll(left, right) }
  }

  //TODO: comments, group by, having, order by
  protected[squealer] def statement = select ~ from ~ where ^^{
    (selectClause, fromClause, whereClause) => SQL(selectClause, fromClause, whereClause)
  }

  val WHERE = """(?i)where""".r
  protected[squealer] def where ={
    WHERE ~ chainExpression ^^{ (_, expr) => Where(expr) } |
    "" ^^{ _ => EmptyWhere }
  }

  val AND = """(?i)and""".r
  val OR = """(?i)or""".r
  protected[squealer] def chainExpression:Parser[List[Expr]] ={
    expression ^^{ e => List(e) } |
    expression ~ AND ~ chainExpression ^^{ (e, _, listOfe) => e :: listOfe } |
    expression ~ OR ~ chainExpression ^^{ (e, _, listOfe) => e :: listOfe }
  }

  protected[squealer] def expression = equation | like | between | in | is

  val ITEM = """[^\s,()]+""".r
  val IS = """(?i)is""".r
  val NOT = """(?i)not""".r
  val NULL = """(?i)null""".r
  protected[squealer] def is ={
    ITEM ~ IS ~ NULL ^^{ (item, _, _) => NullConditional(item) } |
    ITEM ~ IS ~ NOT ~ NULL ^^{ (item, _, _, _) => NullConditional(item) }
  }

  //TODO: nested expressions
  val EQUATION = """[=<>]+""".r
  protected[squealer] def equation = ITEM ~ EQUATION ~ ITEM ^^{ (left, _, right) => Conditional(left, right) }

  val LIKE = """(?i)like""".r
  val STRING = """'.*'""".r
  protected[squealer] def like = ITEM ~ LIKE ~ STRING ^^{ (_, _, _) => Like }

  val BETWEEN = """(?i)between""".r
  val LEFT_PAREN = """\(""".r
  val RIGHT_PAREN = """\)""".r
  val SEP = ",".r
  protected[squealer] def between ={
    ITEM ~ BETWEEN ~ LEFT_PAREN ~ ITEM ~ SEP ~ ITEM ~ RIGHT_PAREN ^^{
      (item, _, _, item1, _, item2, _) => InBetween(item, item1 :: item2 :: Nil)
    }
  }

  //TODO: nested expressions
  val IN = """(?i)in""".r
  protected[squealer] def in = ITEM ~ IN ~ LEFT_PAREN ~ chained ~ RIGHT_PAREN ^^{ (item, _, _, vals, _) => InBetween(item, vals) }

  protected[squealer] def chained:Parser[List[String]] ={
    ITEM ~ SEP ~ chained ^^{ (left, _, more) => left :: more } |
    ITEM ^^{ (item) => List(item) }
  }

  //TODO: nested expressions
  val FROM = """(?i)from""".r
  protected[squealer] def from ={
    FROM ~ aliased ^^{ (_, table) => From(List(table)) } |
    FROM ~ aliased ~ rep(join) ^^{ (_, table, listOfJoin) => From(table :: listOfJoin) }
  }

  //TODO: nested expressions
  val JOIN = """(?i)join""".r
  val INNER = """(?i)inner""".r
  val LEFT = """(?i)left""".r
  val RIGHT = """(?i)right""".r
  val FULL = """(?i)full""".r
  val ON = """(?i)on""".r
  protected[squealer] def join ={
    JOIN ~ aliased ~ ON ~ equation ^^{ (_, item, _, cond) => Join(item, cond) } |
    INNER ~ JOIN ~ aliased ~ ON ~ equation ^^{ (_, _, item, _, cond) => Join(item, cond) } |
    LEFT ~ JOIN ~ aliased ~ ON ~ equation ^^{ (_, _, item, _, cond) => LeftJoin(item, cond) } |
    RIGHT ~ JOIN ~ aliased ~ ON ~ equation ^^{ (_, _, item, _, cond) => RightJoin(item, cond) } |
    FULL ~ JOIN ~ aliased ~ ON ~ equation ^^{ (_, _, item, _, cond) => FullJoin(item, cond) }
  }

  val SELECT = """(?i)select""".r
  protected[squealer] def select = SELECT ~ terms ^^{ (_, t) => Select(t) }

  //TODO: remove wildcard ambiguity
  val WILDCARD = """\*""".r
  protected[squealer] def terms:Parser[List[Expr]] ={
    WILDCARD ^^{_ => List(Wildcard) } |
    distinct ^^{ d => List(d) } |
    distinct ~ SEP ~ terms ^^{ (dist, _, t) => dist :: t } |
    count ^^{ c => List(c) } |
    count ~ SEP ~ terms ^^{ (cnt, _, t) => cnt :: t } |
    aliased ^^{v => List(v)} |
    aliased ~ SEP ~ terms ^^{ (v, _, t) => v :: t }
  }

  val DISTINCT = """(?i)distinct""".r
  val AS = """(?i)as""".r
  protected[squealer] def distinct ={
    DISTINCT ~ LEFT_PAREN ~ ITEM ~ RIGHT_PAREN ^^{ (_, _, item, _) => Term(item, None) } |
    DISTINCT ~ LEFT_PAREN ~ ITEM ~ RIGHT_PAREN ~ AS ~ ITEM ^^{ (_, _, item, _, _, name) => Term(item, Some(name)) }
  }

  val COUNT = """(?i)count""".r
  protected[squealer] def count ={
    COUNT ~ LEFT_PAREN ~ ITEM ~ RIGHT_PAREN ^^{ (_, _, item, _) => Count(item, None) } |
    COUNT ~ LEFT_PAREN ~ ITEM ~ RIGHT_PAREN ~ AS ~ ITEM ^^{ (_, _, item, _, _, name) => Count(item, Some(name)) }
  }

  protected[squealer] def aliased ={
    ITEM ~ AS ~ ITEM ^^{ (term1, _, alias1) => Term(term1, Some(alias1)) } |
    ITEM ^^{ term1 => Term(term1, None) }
  }
}