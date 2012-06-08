package com.wheaties.squealer.sql

import com.wheaties.squealer.db.Database
import com.wheaties.squealer.db.{Column => DBColumn}
import com.wheaties.squealer.db.{Table => DBTable}
import annotation.tailrec
import seekwell._

/**
 * TODO: Order of operations for a basic select clause (without "?")
 *
 * 1) Recursively crawl tree structure. Determine
 *   a. columns used
 *   b. tables used
 * 2) Validate
 *   a. joins
 *   b. where
 *   c. order by
 *   d. group by
 *   e. from
 *   f. columns taken
 * 3) Null columns based on joins
 * 4) Remove primary keys for compound tables
 * 5) produce the Table object
 */

/**
 * Table => alias'd name, use Map[String,String]
 */

//TODO: first make SELECT * FROM tablename work!
//TODO: Options can be handled with a decent forall
class CoalesceSQL(source: Database){
  def apply(sql: String) = StatementParser[Statement](sql) match{
    case select:SelectStatement => Unit
    case insert:InsertStatement => Unit
    case _ => Unit
  }

  def parseSelect(statement: SelectStatement):DBTable = {
    val SelectStatement(distinct, select, from, join, where, groupby, orderby) = statement
    val tableMap = extractTables(from, join)

    where.map(validateWhere(_, tableMap))
    groupby.map(validateGroupBy(_, tableMap))
    orderby.map(validateOrderBy(_, tableMap))

    create(select, tableMap)
  }

  def validateWhere(where: Where, tables: Map[String,DBTable]) = true
  def validateGroupBy(groupBy: GroupBy, tables: Map[String,DBTable]) = true
  def validateOrderBy(orderBy: OrderBy, tables: Map[String,DBTable]) = true

  def create(select: List[Expression], tables: Map[String,DBTable]):DBTable ={
    //TODO: finish me
    DBTable("Finish me", None, Nil)
  }

  //TODO: sub-tables could be formed not in the database but from select statements on the database
  def extractTables(from: List[Table], join: List[Join]):Map[String,DBTable] ={
    val aliasedTables = for{
      table <- from
      aliased <- table.alias
      dbTable <- source.tables.find(_ == table.name)
    } yield (aliased, dbTable)
    val tables = for{
      table <- from
      dbTable <- source.tables.find(_ == table.name)
    } yield (dbTable.name, dbTable)
    val joinTables = for{
      table <- join
      dbTable <- source.tables.find(_ == table.table.name)
    } yield (dbTable.name, dbTable)

    {aliasedTables ::: tables ::: joinTables} toMap
  }

  /*@tailrec final def selectClause(select: List[Expression], columns: List[DBColumn]):List[DBColumn]= select match{
    case Subselect(select) :: xs => selectClause(xs, parseSelect(select) ::: columns)
    case x :: xs => selectClause(xs, parseExpression(x) :: columns)
    case Nil => columns
  }*/

  def parseExpression(expr: Expression, tables: Map[String,DBTable]):List[DBColumn] = expr match{
    case Aliased(expr, alias) => Nil
    case Column(Some(Table(names, _)), name) => Nil
    case Column(None, name) => Nil
    case Wildcard(Some(table)) => Nil
    case Wildcard(None) => tables.values.flatMap(_.columns).toList
    case Function(expr, name) => Nil
    case Negate(expr) => Nil
    case Add(expr1, expr2) => Nil
    case Substract(expr1, expr2) => Nil
    case Multiply(expr1, expr2) => Nil
    case Divide(expr1, expr2) => Nil
    case Subselect(select) => Nil
    case BindParam(name, list) => Nil
    case StringValue(v) => Nil         //TODO: add more data types, like double and such
    case BigDecimalValue(v) => Nil
  }

  //TODO: ask Max how a table could have multiple names - Answer: foo.bar.yo.columnName
  @tailrec final def fromClause(from: List[Table], tables: List[DBTable]):List[DBTable] = from match{
    case Table(List(name), Some(alias)) :: xs => Nil
    case Table(List(name), None) :: xs => Nil
    case Table(names, Some(alias)) :: xs => Nil
    case Table(names, None) :: xs => Nil
    case _ :: xs => fromClause(xs, tables)
    case Nil => tables
  }
}

case class LogicError(msg: String, expr: List[Expression]) extends Exception(msg)
case class LogicWarning(msg: String, expr: List[Expression]) extends Exception(msg)

object ValidateJoinClause{
  //Determines the nullability of columns
  protected[squealer] def joinClause(joins: List[Join]) = joins map{
    case Join(joinKind, table, Some(Left(Using(columns)))) if columns.nonEmpty =>
    case Join(joinKind, table, Some(Right(On(condition)))) =>
    case Join(joinKind, table, None) => //TODO: Natural Joins
  }

  //This just nulls columns, does not actually validate the chosen columns
  protected[squealer] def nullColumns(joinKind: JoinKind, left: DBTable, right: DBTable)= joinKind match{
    case LeftOuter => (left, right) //right to nullable
    case RightOuter => (left, right) //left to nullable
    case FullOuter => (left, right) // both nullable
    case Inner | Equi | Cross | Natural => (left, right) //none changed
  }

  protected[squealer] def parseCondition(condition: Condition, tables: List[DBTable]) = condition match{
    case UnaryCondition(expr, IsNotNull) => Nil
    case UnaryCondition(expr, IsNull) =>    Nil
    case LogicalCondition(condition1, _, condition2) => Nil
    case ComparisonCondition(expr1, Equals, expr2) => Nil
    case ComparisonCondition(expr1, NotEquals, expr2) => Nil
    case ComparisonCondition(expr1, GreaterThan, expr2) => Nil
    case ComparisonCondition(expr1, GreaterThanOrEquals, expr2) => Nil
    case ComparisonCondition(expr1, LessThan, expr2) => Nil
    case ComparisonCondition(expr1, LessThanOrEquals, expr2) => Nil
    case InCondition(expr, inOp, Left(select:SelectStatement)) => Nil
    case InCondition(expr, inOp, Right(exprs)) if exprs.nonEmpty => Nil
    case BetweenCondition(expr, betweenOp, fromExpr, toExpr) => Nil
    case LikeCondition(expr, likeOp, pattern) => Nil
  }
}