package com.wheaties.squealer.sql.validate

import seekwell.GroupBy
import com.wheaties.squealer.db.{Column, Table}
import com.wheaties.squealer.sql.{Result, Failure, Success}
import com.wheaties.squealer.sql.LogicError
import annotation.tailrec

//TODO: find out where we can refactor to reduce code count
//TODO: add the having clause
//TODO: sometimes the groupBy clause column names are prefixed with a table...
class ValidateGroupBy(columnNames: List[String], groupBy: GroupBy) extends (List[Table] => Result[Exception,GroupBy]){

  protected[squealer] def validateColumn(columnName: String, tables: List[Table]):Result[Exception,GroupBy]={
    val columns = for{
      table <- tables
      column <- table.columns if column.name == columnName
    } yield column

    columns match{
      case Nil => Failure(LogicError("No matching column named %s found".format(columnName), groupBy.exprs))
      case List(column) => Success(groupBy)
      case _ => Failure(LogicError("Ambigous column definition, %s,".format(columnName), groupBy.exprs))
    }
  }

  @tailrec final protected[squealer] def validateColumns(columns: List[Result[Exception,GroupBy]],
                                                         result: Result[Exception,GroupBy]):Result[Exception,GroupBy] ={
    columns match{
      case Nil => result
      case x :: xs => validateColumns(xs, result.flatMap(_ => x))
    }
  }

  def apply(tables: List[Table]) ={
    val validations = columnNames.map(validateColumn(_, tables))

    validateColumns(validations, Success(groupBy))
  }
}