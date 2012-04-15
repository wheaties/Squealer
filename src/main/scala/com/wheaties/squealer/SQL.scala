package com.wheaties.squealer

sealed trait Expr
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
case class NullConditional(arg: String) extends Expr
//case class Between(left: String, right: String) extends Expr
case object Like extends Expr
case class SQL(selectClause: Select, fromClause: Expr, whereClause: Expr) extends Expr
case class Union(left: Expr, right: Expr) extends Expr
case class UnionAll(left: Expr, right: Expr) extends Expr