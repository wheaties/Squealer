package com.wheaties.squealer

sealed trait Expr
case object Wildcard extends Expr
case class Term(term: String, alias: Option[String]) extends Expr
case class Count(term: String, alias: Option[String]) extends Expr
case class Select(terms: List[Expr]) extends Expr
case class From(clauses: List[Expr]) extends Expr

sealed trait JoinExpr extends Expr{
  def term: Expr
  def cond: Expr
}
case class Join(term: Expr, cond: Expr) extends JoinExpr
case class LeftJoin(term: Expr, cond: Expr) extends JoinExpr
case class RightJoin(term: Expr, cond: Expr) extends JoinExpr
case class FullJoin(term: Expr, cond: Expr) extends JoinExpr

sealed trait WhereExpr extends Expr
case class Where(clauses: List[Expr]) extends WhereExpr
case object EmptyWhere extends WhereExpr

case class Conditional(left: String, right: String) extends Expr
case class InBetween(arg: String, items: List[String]) extends Expr
case class NullConditional(arg: String) extends Expr
case object Like extends Expr

case class SQL(selectClause: Select, fromClause: From, whereClause: WhereExpr) extends Expr
case class Union(left: Expr, right: Expr) extends Expr