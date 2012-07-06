package com.wheaties.squealer.sql

import org.eclipse.datatools.sqltools.parsers.sql.query._
import org.eclipse.datatools.sqltools.parsers.sql.SQLParserException
import org.eclipse.datatools.modelbase.sql.query.helper.StatementHelper
import org.eclipse.datatools.modelbase.sql.query.{ValueExpressionColumn, QuerySelectStatement}
import com.wheaties.squealer.db.{UnknownType, ColumnDef, Column}
import java.util.Properties
import java.sql.Connection

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

  def connectionProperties(con: Connection)={
    val prop = new Properties()
    prop.setProperty(IDriverMgmtConstants.PROP_DEFN_JARLIST, jarlist)
    prop.setProperty(IJDBCConnectionProfileConstants.URL_PROP_ID, driverUrl)
    prop.setProperty(IJDBCConnectionProfileConstants.USERNAME_PROP_ID, user)
    prop.setProperty(IJDBCConnectionProfileConstants.PASSWORD_PROP_ID, password)
    prop.setProperty()
    prop.setProperty()
    prop.setProperty()
    prop.setProperty()
    prop.setProperty()
    val meta = con.getMetaData
    meta.getDatabaseProductVersion
    meta.getDriverName
    meta.getDriverVersion
    meta.getURL
    meta.getUserName
  }

  val BIGINT = "(?i)BIGINT".r
  val DOUBLE = "(?i)DOUBLE".r
  val FLOAT = "(?i)FLOAT".r
  val INTEGER = "(?i)INTEGER".r
  val NUMERIC = "(?i)NUMERIC".r
  val DECIMAL = "(?i)DECIMAL".r
  val REAL = "(?i)REAL".r
  val BIT = "(?i)BIT".r
  val SMALLINT = "(?i)SMALLINT".r
  val TINYINT = "(?i)TINYINT".r
  val CHAR = "(?i)CHAR".r
  val LONGVARCHAR = "(?i)LONGVARCHAR".r
  val VARCHAR = "(?i)VARCHAR".r
  val LONGNVARCHAR = "(?i)LONGNVARCHAR".r
  val NCHAR = "(?i)NCHAR".r
  val BINARY = "(?i)BINARY".r
  val VARBINARY = "(?i)VARBINARY".r
  val LONGVARBINARY = "(?i)LONGVARBINARY".r
  val DATE = "(?i)DATE".r
  val TIME = "(?i)TIME".r
  val TIMESTAMP = "(?i)TIMESTAMP".r
  val CLOB = "(?i)CLOB".r
  val BLOB = "(?i)BLOB".r
  val ARRAY = "(?i)ARRAY".r
}