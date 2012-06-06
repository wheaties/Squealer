package com.wheaties.squealer.sql.validate

import com.wheaties.squealer.db.{Column => DBColumn, Table => DBTable}
import seekwell._
import com.wheaties.squealer.sql.{LogicError, Success, Failure, Result}

class ValidateUnaryCondition(condition: UnaryCondition) extends (List[DBTable] => Result[Exception,Condition]){

  val UnaryCondition(expr, _) = condition

  protected[squealer] def validateColumn(columnName: String, tableName: Option[String], tables: List[DBTable]) ={
    val columns = for{
      table <- tables if tableName.forall(_ == table.name)
      column <- table.columns if column.name == columnName
    } yield column

    columns match{
      case Nil => Failure(LogicError("No matching column named %s found".format(columnName), condition.exprs))
      case List(column) => Success(column)
      case _ => Failure(LogicError("Ambigous column definition, %s,".format(columnName), condition.exprs))
    }
  }

  protected[squealer] def parseExpression(expression: Expression, tables: List[DBTable]):Result[Exception,Expression] ={
    expression match{
      case Aliased(expr1, alias) => for{ resExpr <- parseExpression(expr1, tables) } yield expression
      case Column(table, columnName) =>
        for{ resCol <- validateColumn(columnName, table.map(_.name.mkString(".")), tables) } yield expression
      case Wildcard(Some(table)) => Failure(LogicError("%s.* is not an acceptable condition on a comparison".format(table.name.mkString(".")), condition.exprs))
      case Wildcard(None) => Failure(LogicError("* is not an acceptable condition on a comparison", condition.exprs))
      case Function(_, name) => Failure(LogicError("%s can not be processed by Squealer".format(name), condition.exprs))
      case Negate(_) => Failure(LogicError("Mathematical operators can not be used on a null check", condition.exprs))
      case expr:BinaryAlgebraicExpression =>
        Failure(LogicError("Mathematical operators can not be used on a null check", condition.exprs))
      case BindParam(name, list) =>
          Failure(LogicError("A parameter can not be bound in a null check".format(name), condition.exprs))
      case x:Subselect => Failure(LogicError("%s can not be processed by Squealer".format(x.toString()), condition.exprs))
    }
  }

  def apply(tables: List[DBTable])= for{ resExpr <- parseExpression(expr, tables) } yield condition
}

//TODO: tables/columns can be aliased!
class ValidateComparisonCondition(condition: ComparisonCondition) extends (List[DBTable] => Result[Exception,Condition]){

  protected val ComparisonCondition(expr1, _, expr2) = condition

  def validateArithmetic(columnName: String, tableName: String) {}

  protected[squealer] def validateColumn(columnName: String, tableName: Option[String], tables: List[DBTable]) ={
    val columns = for{
      table <- tables if tableName.forall(_ == table.name)
      column <- table.columns if column.name == columnName
    } yield column

    columns match{
      case Nil => Failure(LogicError("No matching column named %s found".format(columnName), condition.exprs))
      case List(column) => Success(column)
      case _ => Failure(LogicError("Ambigous column definition, %s,".format(columnName), condition.exprs))
    }
  }

  //TODO: yeah, really need to have constants for the jdbc datatypes...
  protected[squealer] def validateColType(column: DBColumn) = column.typeOf match{
    case _ => Success("Finish me") //TODO: match on mathematical types
  }

  //TODO: perhaps can orElse some of these so that we can have a single expression parser
  protected[squealer] def parseExpression(expression: Expression, tables: List[DBTable]):Result[Exception,Expression] ={
    expression match{
      case Aliased(expr, alias) => Failure(LogicError("Aliasing is not supported on a comparison", condition.exprs))
      case Column(table, columnName) =>
        for{
          resCol <- validateColumn(columnName, table.map(_.name.mkString(".")), tables)
          resExpr <- validateColType(resCol)
        } yield expression
      case Wildcard(Some(table)) =>
        Failure(LogicError("%s.* is not an acceptable condition on a comparison".format(table.name.mkString(".")), condition.exprs))
      case Wildcard(None) => Failure(LogicError("* is not an acceptable condition on a comparison", condition.exprs))
      case Function(expr, name) => Failure(LogicError("%s can not be processed by Squealer".format(name), condition.exprs))
      case Negate(expr) => for{ resExpr <- parseExpression(expr, tables) } yield expression
      case expr:BinaryAlgebraicExpression =>
        for{
          resExpr1 <- parseExpression(expr.left, tables)
          resExpr2 <- parseExpression(expr.right, tables)
        } yield expression
      case x:BindParam => Success(expression)
      case x:Subselect => Failure(LogicError("%s can not be processed by Squealer".format(x.toString()), condition.exprs))
    }
  }

  //TODO: how am I do have Partial w/ warning about data conversions?
  def apply(tables: List[DBTable]) = for{
    resExpr1 <- parseExpression(expr1, tables)
    resExpr2 <- parseExpression(expr2, tables)
  } yield condition
}

class ValidateLikeCondition(condition: LikeCondition) extends (List[DBTable] => Result[Exception,Condition]){
  val LikeCondition(expr, _, _) = condition

  protected[squealer] def validateColType(column: DBColumn) = column.typeOf match{
    case _ => Success("Finish me") //TODO: match on string types
  }

  protected[squealer] def parseExpression(expression: Expression, tables: List[DBTable]):Result[Exception,Expression] ={
    expression match{
      case Aliased(expr, alias) => Failure(LogicError("Aliasing is not supported on a comparison", condition.exprs))
      case Column(table, columnName) =>
        for{
          resCol <- validateColumn(columnName, table.map(_.name.mkString(".")), tables)
          resExpr <- validateColType(resCol)
        } yield expression
      case BindParam(name, list) => Success(expression)
      case Wildcard(Some(table)) => Failure(LogicError("%s.* is not an acceptable condition on a comparison".format(table.name.mkString(".")), condition.exprs))
      case Wildcard(None) => Failure(LogicError("* is not an acceptable condition on a comparison", condition.exprs))
      case Function(expr, name) => Failure(LogicError("%s can not be processed by Squealer".format(name), condition.exprs))
      case Negate(expr) => Failure(LogicError("Mathematical operators can not be used in like conditions", condition.exprs))
      case x:BinaryAlgebraicExpression => Failure(LogicError("Mathematical operators can not be used in like conditions", condition.exprs))
      case x:Subselect => Failure(LogicError("%s can not be processed by Squealer".format(x.toString()), condition.exprs))
    }
  }

  protected[squealer] def validateColumn(columnName: String, tableName: Option[String], tables: List[DBTable]) ={
    val columns = for{
      table <- tables if tableName.forall(_ == table.name)
      column <- table.columns if column.name == columnName
    } yield column

    columns match{
      case Nil => Failure(LogicError("No matching column named %s found".format(columnName), condition.exprs))
      case List(column) => Success(column)
      case _ => Failure(LogicError("Ambigous column definition, %s,".format(columnName), condition.exprs))
    }
  }

  def apply(tables: List[DBTable])= for{ resExpr <- parseExpression(expr, tables) } yield condition
}

//TODO: oh the D-R-Y violations...
class ValidateInCondition(condition: InCondition) extends (List[DBTable] => Result[Exception,Condition]){
  val InCondition(expr, _, statements) = condition

  def apply(tables: List[DBTable]) = Success(condition)
}