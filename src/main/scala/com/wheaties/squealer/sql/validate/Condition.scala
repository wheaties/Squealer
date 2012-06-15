package com.wheaties.squealer.sql.validate

import seekwell._
import com.wheaties.squealer.sql._
import annotation.tailrec
import com.wheaties.squealer.db.{StringType, UnknownType, DataType, Column => DBColumn, Table => DBTable}

//TODO: Forgot about constants!
//TODO: tables/columns can be aliased!
//TODO: pull out parseExpression into a nice package partial function. Then just use what I need.
//TODO: Perhaps think about passing in condition type name or some other abstract class.
//TODO: turn these into objects that return traits!
class ValidateUnaryCondition(condition: UnaryCondition) extends (List[DBTable] => Result[Exception,Condition]){

  protected val UnaryCondition(expr, _) = condition
  protected val validateCol = validateColumn(condition.exprs) _

  protected[squealer] def parseExpression(expression: Expression, tables: List[DBTable]):Result[Exception,DataType] ={
    expression match{
      case Aliased(expr1, alias) => Failure(LogicError("Aliasing is not supported on a comparison", condition.exprs))
      case Column(table, columnName) =>
        for{ resCol <- validateCol(columnName, table.map(_.name.mkString(".")), tables) } yield resCol.typeOf
      case Wildcard(Some(table)) => Failure(LogicError("%s.* is not an acceptable condition on a comparison".format(table.name.mkString(".")), condition.exprs))
      case Wildcard(None) => Failure(LogicError("* is not an acceptable condition on a comparison", condition.exprs))
      case Function(_, name) => Failure(LogicError("%s can not be processed by Squealer".format(name), condition.exprs))
      case x:Negate => Failure(LogicError("Mathematical operators can not be used on a null check", condition.exprs))
      case x:BinaryAlgebraicExpression => Failure(LogicError("Mathematical operators can not be used on a null check", condition.exprs))
      case BindParam(name, _) => Failure(LogicError("A %s parameter can not be bound in a null check".format(name), condition.exprs))
      case x:Subselect => Failure(LogicError("%s can not be processed by Squealer".format(x.toString()), condition.exprs))
    }
  }

  def apply(tables: List[DBTable])= for{ resColType <- parseExpression(expr, tables) } yield condition
}

class ValidateComparisonCondition(condition: ComparisonCondition) extends (List[DBTable] => Result[Exception,Condition]){

  protected val ComparisonCondition(expr1, operator, expr2) = condition
  protected val validateCol = validateColumn(condition.exprs) _

  protected[squealer] def validateColType(column: DBColumn) = operator match{
    case Equals | NotEquals => Success(column.typeOf)
    case _ if column.typeOf.isNumeric => Success(column.typeOf)
    case _ => Failure(LogicError("Non-numerical column types can't be compared".format(column.typeOf), condition.exprs))
  }

  protected[squealer] def parseExpression(expression: Expression, tables: List[DBTable]):Result[Exception,DataType] ={
    expression match{
      case Aliased(expr, alias) => Failure(LogicError("Aliasing is not supported on a comparison", condition.exprs))
      case Column(table, columnName) =>
        for{
          resCol <- validateCol(columnName, table.map(_.name.mkString(".")), tables)
          resType <- validateColType(resCol)
        } yield resType
      case Wildcard(Some(table)) =>
        Failure(LogicError("%s.* is not an acceptable condition on a comparison".format(table.name.mkString(".")), condition.exprs))
      case Wildcard(None) => Failure(LogicError("* is not an acceptable condition on a comparison", condition.exprs))
      case Function(expr, name) => Failure(LogicError("%s can not be processed by Squealer".format(name), condition.exprs))
      case Negate(expr) => for{
        resType <- parseExpression(expr, tables)
      } yield resType //TODO: Check if numeric
      case expr:BinaryAlgebraicExpression =>
        for{
          resType1 <- parseExpression(expr.left, tables)
          resType2 <- parseExpression(expr.right, tables)
        } yield resType1 //TODO: partial success right here if different data types
      case x:BindParam => Success(UnknownType)
      case x:Subselect => Failure(LogicError("%s can not be processed by Squealer".format(x.toString()), condition.exprs))
      case x:StringValue => Success(StringType)
    }
  }

  //TODO: get in the data type checks
  def apply(tables: List[DBTable]) = for{
    resType1 <- parseExpression(expr1, tables)
    resType2 <- parseExpression(expr2, tables)
  } yield condition
}

//TODO: add string/bigdecimal etc. column types to Max's Seekwell
class ValidateLikeCondition(condition: LikeCondition) extends (List[DBTable] => Result[Exception,Condition]){

  protected val LikeCondition(expr, _, _) = condition
  protected val validateCol = validateColumn(condition.exprs) _

  protected[squealer] def validateColType(column: DBColumn) = if(column.typeOf.isString){
    Success(DBColumn)
  }
  else{
    Failure(LogicError("Column types of %s can not be compared via Like expressions".format(column.typeOf), condition.exprs))
  }

  protected[squealer] def parseExpression(expression: Expression, tables: List[DBTable]):Result[Exception,DataType] ={
    expression match{
      case x:Aliased => Failure(LogicError("Aliasing is not supported on a comparison", condition.exprs))
      case Column(table, columnName) =>
        for{
          resCol <- validateCol(columnName, table.map(_.name.mkString(".")), tables)
          resExpr <- validateColType(resCol)
        } yield resCol.typeOf
      case x:BindParam => Success(UnknownType)
      case Wildcard(Some(table)) => Failure(LogicError("%s.* is not an acceptable condition on a comparison".format(table.name.mkString(".")), condition.exprs))
      case Wildcard(None) => Failure(LogicError("* is not an acceptable condition on a comparison", condition.exprs))
      case Function(_, name) => Failure(LogicError("%s can not be processed by Squealer".format(name), condition.exprs))
      case x:Negate => Failure(LogicError("Mathematical operators can not be used in like conditions", condition.exprs))
      case x:BinaryAlgebraicExpression => Failure(LogicError("Mathematical operators can not be used in like conditions", condition.exprs))
      case x:Subselect => Failure(LogicError("%s can not be processed by Squealer".format(x.toString()), condition.exprs))
      case x:StringValue => Success(StringType)
    }
  }

  def apply(tables: List[DBTable])= for{ resExpr <- parseExpression(expr, tables) } yield condition
}

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