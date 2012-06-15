package com.wheaties.squealer.sql.validate

import com.wheaties.squealer.sql.{Result, Failure, Success}
import com.wheaties.squealer.sql.LogicError
import seekwell._
import com.wheaties.squealer.db.{DataType, Table, Column => DBColumn}

object `package` {


  protected[squealer] def validateColumn(exprs: List[Expression])(columnName: String,
                                                                  tableName: Option[String],
                                                                  tables: List[Table]):Result[Exception,DBColumn] ={
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

  type ExpressionParser = PartialFunction[Expression, Result[Exception,DataType]]

  protected[squealer] def failExpression(exprs: List[Expression], evalType: String):ExpressionParser={
    case x:Aliased => Failure(LogicError("Aliasing is not supported on %s".format(evalType), exprs))
    case x:Column => Failure(LogicError("Strings can not be used on %s".format(evalType), exprs))
    case Wildcard(Some(table)) => Failure(LogicError("%s.* is not an acceptable condition on %s".format(table.name.mkString("."), evalType), exprs))
    case Wildcard(None) => Failure(LogicError("* is not an acceptable condition on %s".format(evalType), exprs))
    case Function(_, name) => Failure(LogicError("%s can not be processed by Squealer".format(name), exprs))
    case x:Negate => Failure(LogicError("Mathematical operators can not be used on %s".format(evalType), exprs))
    case x:BinaryAlgebraicExpression => Failure(LogicError("Mathematical operators can not be used on %s".format(evalType), exprs))
    case BindParam(name, _) => Failure(LogicError("A %s parameter can not be bound on %s".format(name, evalType), exprs))
    case x:Subselect => Failure(LogicError("%s can not be processed by Squealer".format(x.toString()), exprs))
    case x:StringValue => Failure(LogicError("Strings can not be used on %s".format(evalType), exprs))
  }
}