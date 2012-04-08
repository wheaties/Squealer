package com.wheaties.squealer

import com.codecommit.gll._

//TODO: move these to another file
trait Expr
case object Wildcard extends Expr
case class Term(term: String, alias: Option[String]) extends Expr
case class Select(terms: List[Expr]) extends Expr
case object From extends Expr //TODO: finish me
case object Where extends Expr //TODO: finish me
case class SQL(selectClause: Select, fromClause: Expr, whereClause: Expr) extends Expr

object SQLParser extends Parsers with RegexParsers{

  val SELECT = """(?i)select""".r
  val AS = """(?i)as""".r
  val FROM = """(?i)from""".r
  val WHERE = """(?i)where""".r
  val ITEM = """[^ ,]+""".r
  val SEP = """[\s,]+""".r

  def statement = select ~ from ~ where ^^{
    (selectClause, fromClause, whereClause) => SQL(selectClause, fromClause, whereClause)
  }

  def select = SELECT ~ terms ^^{ (_, t) => Select(t) }

  def where ={
    WHERE ^^{ _ => Where } |
    "" ^^{ _ => Where }
  }


  def from = FROM ^^{ _ => From }

  def terms:Parser[List[Expr]] ={
    variable ^^{v => List(v)} |
    variable ~ SEP ~ terms ^^{ (v, _, t) => v :: t }
  }

  //TODO: remove wildcard ambiguity
  def variable:Parser[Expr] ={
      "\\*".r ^^{_ => Wildcard} |
      ITEM ~ AS ~ ITEM ^^{ (term1, _, alias1) => Term(term1, Some(alias1)) } |
      ITEM ^^{ term1 => Term(term1, None) }
  }
}