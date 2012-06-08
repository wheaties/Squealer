package com.wheaties.squealer.sql.validate

import seekwell._
import com.wheaties.squealer.sql._
import annotation.tailrec
import com.wheaties.squealer.db.{UnknownType, DataType, Column => DBColumn, Table => DBTable}

//TODO: Refactor this code!

class ValidateUnaryCondition(condition: UnaryCondition) extends (List[DBTable] => Result[Exception,Condition]){

  protected val UnaryCondition(expr, _) = condition
  protected val validateCol = validateColumn(condition.exprs) _

  protected[squealer] def parseExpression(expression: Expression, tables: List[DBTable]):Result[Exception,Expression] ={
    expression match{
      case Aliased(expr1, alias) => Failure(LogicError("Aliasing is not supported on a comparison", condition.exprs))
      case Column(table, columnName) =>
        for{ resCol <- validateCol(columnName, table.map(_.name.mkString(".")), tables) } yield expression
      case Wildcard(Some(table)) => Failure(LogicError("%s.* is not an acceptable condition on a comparison".format(table.name.mkString(".")), condition.exprs))
      case Wildcard(None) => Failure(LogicError("* is not an acceptable condition on a comparison", condition.exprs))
      case Function(_, name) => Failure(LogicError("%s can not be processed by Squealer".format(name), condition.exprs))
      case x:Negate => Failure(LogicError("Mathematical operators can not be used on a null check", condition.exprs))
      case x:BinaryAlgebraicExpression => Failure(LogicError("Mathematical operators can not be used on a null check", condition.exprs))
      case BindParam(name, _) => Failure(LogicError("A %s parameter can not be bound in a null check".format(name), condition.exprs))
      case x:Subselect => Failure(LogicError("%s can not be processed by Squealer".format(x.toString()), condition.exprs))
    }
  }

  def apply(tables: List[DBTable])= for{ resExpr <- parseExpression(expr, tables) } yield condition
}

//TODO: tables/columns can be aliased!
class ValidateComparisonCondition(condition: ComparisonCondition) extends (List[DBTable] => Result[Exception,Condition]){

  protected val ComparisonCondition(expr1, operator, expr2) = condition
  protected val validateCol = validateColumn(condition.exprs) _

  //TODO: equality checks can support any type, maths can't
  protected[squealer] def validateColType(column: DBColumn) = if(column.typeOf.isNumeric){
    Success(column)
  }
  else{
    Failure(LogicError("Non-numerical column types are not supported in ".format(column.typeOf), condition.exprs))
  }

  protected[squealer] def parseExpression(expression: Expression, tables: List[DBTable]):Result[Exception,Expression] ={
    expression match{
      case Aliased(expr, alias) => Failure(LogicError("Aliasing is not supported on a comparison", condition.exprs))
      case Column(table, columnName) =>
        for{
          resCol <- validateCol(columnName, table.map(_.name.mkString(".")), tables)
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

//TODO: add string/bigdecimal etc. column types to Max's Seekwell
//TODO: conditions are expression1 types...
class ValidateLikeCondition(condition: LikeCondition) extends (List[DBTable] => Result[Exception,Condition]){

  protected val LikeCondition(expr, _, _) = condition
  protected val validateCol = validateColumn(condition.exprs) _

  protected[squealer] def validateColType(column: DBColumn) = if(column.typeOf.isString){
    Success(DBColumn)
  }
  else{
    Failure(LogicError("Column types of %s can not be compared via Like expressions".format(column.typeOf), condition.exprs))
  }

  protected[squealer] def parseExpression(expression: Expression, tables: List[DBTable]):Result[Exception,Expression] ={
    expression match{
      case x:Aliased => Failure(LogicError("Aliasing is not supported on a comparison", condition.exprs))
      case Column(table, columnName) =>
        for{
          resCol <- validateCol(columnName, table.map(_.name.mkString(".")), tables)
          resExpr <- validateColType(resCol)
        } yield expression
      case x:BindParam => Success(expression)
      case Wildcard(Some(table)) => Failure(LogicError("%s.* is not an acceptable condition on a comparison".format(table.name.mkString(".")), condition.exprs))
      case Wildcard(None) => Failure(LogicError("* is not an acceptable condition on a comparison", condition.exprs))
      case Function(_, name) => Failure(LogicError("%s can not be processed by Squealer".format(name), condition.exprs))
      case x:Negate => Failure(LogicError("Mathematical operators can not be used in like conditions", condition.exprs))
      case x:BinaryAlgebraicExpression => Failure(LogicError("Mathematical operators can not be used in like conditions", condition.exprs))
      case x:Subselect => Failure(LogicError("%s can not be processed by Squealer".format(x.toString()), condition.exprs))
    }
  }

  def apply(tables: List[DBTable])= for{ resExpr <- parseExpression(expr, tables) } yield condition
}

//TODO: oh the D-R-Y violations...
class ValidateInCondition(condition: InCondition) extends (List[DBTable] => Result[Exception,Condition]){

  protected val InCondition(expr, _, statements) = condition
  protected val validateCol = validateColumn(condition.exprs) _

  //TODO: figure out subselects, 'cause that's half the reason people do "in" clauses
  protected[squealer] def parseExpression(expression: Expression, tables: List[DBTable]):Result[Exception,DataType] ={
    expression match{
      case Aliased(expr, alias) => Failure(LogicError("Aliasing is not supported on a comparison", condition.exprs))
      case Column(table, columnName) => validateCol(columnName, table.map(_.name.mkString(".")), tables).map(_.typeOf)
      case BindParam(name, _) => Partial(UnknownType, LogicWarning("Unable to verify expression type", condition.exprs))
      case Wildcard(Some(table)) => Failure(LogicError("%s.* is not an acceptable condition on a comparison".format(table.name.mkString(".")), condition.exprs))
      case Wildcard(None) => Failure(LogicError("* is not an acceptable condition on a comparison", condition.exprs))
      case Function(expr, name) => Failure(LogicError("%s can not be processed by Squealer".format(name), condition.exprs))
      case Negate(expr) => Failure(LogicError("Mathematical operators can not be used in like conditions", condition.exprs))
      case x:BinaryAlgebraicExpression => Failure(LogicError("Mathematical operators can not be used in like conditions", condition.exprs))
      case x:Subselect => Failure(LogicError("%s can not be processed by Squealer".format(x), condition.exprs))
    }
  }

  @tailrec final protected[squealer] def validateColumns(columns: List[Result[Exception,Condition]],
                                                         result: Result[Exception,Condition]):Result[Exception,Condition] ={
    columns match{
      case Nil => result
      case x :: xs => validateColumns(xs, result.flatMap(_ => x))
    }
  }

  protected[squealer] def validateExpressions(expressions: List[Expression], tables: List[DBTable])={
    val resColType = parseExpression(expr, tables)
    val results = for{ expression <- expressions } yield{
      val resExprType = parseExpression(expression, tables)
      if(resColType == resExprType){
        Success(condition)
      }
      else{
        Failure(LogicError("%s does not match column type %s".format(resExprType, resColType), condition.exprs))
      }
    }

    validateColumns(results, Success(condition))
  }

  def apply(tables: List[DBTable]) = statements match{
    case Right(Nil) => Failure(LogicError("In conditions must contain matching selections", condition.exprs))
    case Right(expressions) => validateExpressions(expressions, tables)
    case Left(subselect) => Failure(LogicError("%s can not be processed by Squealer".format(subselect), condition.exprs))
  }
}