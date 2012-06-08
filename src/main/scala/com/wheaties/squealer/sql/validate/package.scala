package com.wheaties.squealer.sql.validate

import com.wheaties.squealer.db.{Table, Column}
import com.wheaties.squealer.sql.{Result, Failure, Success}
import com.wheaties.squealer.sql.LogicError
import seekwell.Expression

object `package` {
   protected[squealer] def validateColumn(exprs: List[Expression])(columnName: String,
                                                                  tableName: Option[String],
                                                                  tables: List[Table]):Result[Exception,Column] ={
    val columns = for{
      table <- tables if tableName.forall(_ == table.name)
      column <- table.columns if column.name == columnName
    } yield column

    columns match{
      case Nil => Failure(LogicError("No matching column named %s found".format(columnName), exprs))
      case List(column) => Success(column)
      case _ => Failure(LogicError("Ambigous column definition, %s,".format(columnName), exprs))
    }
  }
}