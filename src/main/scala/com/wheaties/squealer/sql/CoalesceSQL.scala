package com.wheaties.squealer.sql

import com.wheaties.squealer.db.Database
import annotation.tailrec
import seekwell._

//TODO: handle package name collisions between Seekwell and Squealer

//TODO: first make SELECT * FROM tablename work!
object CoalesceSQL{
  def apply(source: Database, sql: String) ={
    //TODO: Options can be handled with a decent forall
    StatementParser[Statement](sql) match{
      case SelectStatement(distinct, select, from, join, Some(where), Some(groupby), orderby) => Unit
      case SelectStatement(distinct, select, from, join, Some(where), _, orderby) => Unit
      case SelectStatement(distinct, select, from, join, _, Some(groupby), orderby) => Unit
      case SelectStatement(distinct, select, from, join, _, _, orderby) => Unit
      case InsertStatement(table, columns, values) => Unit
      case _ => Unit
    }
  }

  //TODO: don't forget about counts
  @tailrec final def selectClause(select: List[Expression], some: List[Int]):List[Int]={
    select match{
      case Aliased(expr, alias) :: xs =>
      case Column(Some(table), name) :: xs =>
      case Column(None, name) :: xs =>
      case Wildcard(Some(table)) :: xs =>
      case Wildcard(None) :: xs =>
      case Function(expr, name) :: xs =>
      case _ :: xs => selectClause(xs, some)
      case Nil => some
    }
  }

  //TODO: ask Max if I even need to worry about these
  @tailrec final def fromClause(from: List[Table], tables: List[Int]):List[Int] ={
    from match{
      case Table(names, Some(alias)) =>
      case Table(names, None) =>
      case Table(List(name), Some(alias)) =>
      case Table(List(name), None) =>
      case _ :: xs => fromClause(xs, tables)
      case Nil => tables
    }
  }

  //Determines the nullability of columns
  protected[squealer] def joinClause(joins: List[Join])={
    joins map{
      case Join(joinKind, table, Some(Using(columns)) =>
      case Join(joinKind, table, Some(On(condition))) =>
      case Join(joinKind, table, None) => //TODO: ask Max what exactly this means
    }
  }
}