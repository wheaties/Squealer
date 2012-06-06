package com.wheaties.squealer.sql.validate

import seekwell.Join
import com.wheaties.squealer.db.{Column, Table}
import com.wheaties.squealer.sql.{Result, Failure, Success}
import com.wheaties.squealer.sql.LogicError
import annotation.tailrec

//These need to hold only the logic to validate, (not to parse the expression?)
class ValidateJoin(leftTable: String, rightTable: String, columnName: String, join: Join)
    extends (List[Table] => Result[Exception,Join]){

  protected[squealer] def validateColumn(tableName: String, tables: List[Table]):Result[Exception,Column] ={
    val columns = for{
      table <- tables if table.name == tableName
      column <- table.columns if column.name == columnName
    } yield column

    columns match{
      case Nil => Failure(LogicError("No matching column named %s found".format(columnName), join.exprs))
      case List(column) => Success(column)
      case _ => Failure(LogicError("Ambigous column definition, %s,".format(columnName), join.exprs))
    }
  }

  protected[squealer] def validateCondition(left: Column, right: Column)=if(left.colType == right.colType){
    Success(join)
  }
  else{
    Failure(LogicError("%s does not match %s".format(left.colType, right.colType), join.exprs))
  }

  def apply(tables: List[Table]):Result[Exception,Join] = for{
    left <- validateColumn(leftTable, tables)
    right <- validateColumn(rightTable, tables)
    validated <- validateCondition(left, right)
  } yield validated
}

class ValidateNaturalJoin(leftTable: String, rightTable: String, join: Join)
    extends (List[Table] => Result[Exception,Join]){

  protected def extractColumns(tableName: String, tables: List[Table]) = for{
      table <- tables if table.name == tableName
      column <- table.columns
    } yield column

  @tailrec final protected[squealer] def validateColumns(columns: List[Result[Exception,Join]],
                                                         result: Result[Exception,Join]):Result[Exception,Join] ={
    columns match{
      case Nil => result
      case x :: xs => validateColumns(xs, result.flatMap(_ => x))
    }
  }

  def apply(tables: List[Table]) ={
    val leftColumns = extractColumns(leftTable, tables)
    val rightColumns = extractColumns(rightTable, tables)

    val columns = for{
      right <- rightColumns
      left <- leftColumns if right.name == left.name
    } yield{
      if(left.colType == right.colType){
        Success(join)
      }
      else{
        val msg = "%s of tables %s and %s is not the same type".format(left.name, leftTable, rightTable)
        Failure(LogicError(msg, join.exprs))
      }
    }

    if(columns.isEmpty){
      Failure(LogicError("%s and %s do not share any column names".format(leftTable, rightTable), join.exprs))
    }
    else{
      validateColumns(columns, Success(join))
    }
  }
}