package com.wheaties.squealer.sql

import org.eclipse.datatools.sqltools.parsers.sql.query._
import org.eclipse.datatools.sqltools.parsers.sql.SQLParserException
import org.eclipse.datatools.modelbase.sql.query.helper.StatementHelper
import org.eclipse.datatools.modelbase.sql.query.{ValueExpressionColumn, QuerySelectStatement}
import com.wheaties.squealer.db.{UnknownType, ColumnDef, Column}

//TODO: figure out a way to map the constraints of a column within the db to the columns here.
class StatementParser {

  //TODO: don't I need a JDBC connection here to power this thing up?
  def apply(statement: String)= try{
    val manager = SQLQueryParserManagerProvider.getInstance().getParserManager(null, null)
    val parsed = manager.parseQuery(statement)
    val stmt = parsed.getQueryStatement.asInstanceOf[QuerySelectStatement]
    val columns = StatementHelper.getEffectiveResultColumns(stmt).iterator;
    while(columns.hasNext){
      val single = columns.next().asInstanceOf[ValueExpressionColumn]
      val datatype = single.getDataType.getName
      val name = single.getName

      //TODO: map Java "stringly-typed" goodness to concrete types.
      Column(name, UnknownType, None, None, ColumnDef)
    }
  }
  catch{
    case ex: SQLParserException => Failure(ex)
    case ex: Exception => Failure(ex)
  }

  val BIGINT = "(?i)BIGINT".r
  val DOUBLE = "(?i)DOUBLE".r
  val FLOAT = "(?i)FLOAT".r
  val INTEGER = "(?i)INTEGER".r
  val NUMERIC = "(?i)NUMERIC".r
  val DECIMAL = "(?i)DECIMAL".r
  val VARCHAR = "(?i)VARCHAR".r

}