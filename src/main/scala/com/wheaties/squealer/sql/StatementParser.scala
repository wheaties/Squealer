package com.wheaties.squealer.sql

import org.eclipse.datatools.sqltools.parsers.sql.query._
import org.eclipse.datatools.sqltools.parsers.sql.SQLParserException
import org.eclipse.datatools.modelbase.sql.query.helper.StatementHelper
import org.eclipse.datatools.modelbase.sql.query.{ValueExpressionColumn, QuerySelectStatement}
import java.util.Properties
import java.sql.Connection
import scala.util.parsing.combinator.RegexParsers
import com.wheaties.squealer.db._

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
      val datatype = SqlToolsDataType(single.getDataType.getName)
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
    //prop.setProperty(IDriverMgmtConstants.PROP_DEFN_JARLIST, jarlist)
    //prop.setProperty(IJDBCConnectionProfileConstants.URL_PROP_ID, driverUrl)
    //prop.setProperty(IJDBCConnectionProfileConstants.USERNAME_PROP_ID, user)
    //prop.setProperty(IJDBCConnectionProfileConstants.PASSWORD_PROP_ID, password)
    //prop.setProperty()
    //prop.setProperty()
    //prop.setProperty()
    //prop.setProperty()
    //prop.setProperty()
    val meta = con.getMetaData
    meta.getDatabaseProductVersion
    meta.getDriverName
    meta.getDriverVersion
    meta.getURL
    meta.getUserName
  }
}

object SqlToolsDataType extends RegexParsers with (String => DataType){

  protected def BIGINT = regex("(?i)BIGINT".r) ^^ {_ => LongType }
  protected def DOUBLE = regex("(?i)DOUBLE".r) ^^ {_ => DoubleType }
  protected def FLOAT = regex("(?i)FLOAT".r) ^^ {_ => FloatType }
  protected def INTEGER = regex("(?i)INTEGER".r) ^^ {_ => IntType }
  protected def NUMERIC = regex("(?i)NUMERIC".r) ^^ {_ => DecimalType }
  protected def DECIMAL = regex("(?i)DECIMAL".r) ^^ {_ => DecimalType }
  protected def REAL = regex("(?i)REAL".r) ^^ {_ => FloatType }
  protected def BIT = regex("(?i)BIT".r) ^^ {_ => BooleanType }
  protected def SMALLINT = regex("(?i)SMALLINT".r) ^^ {_ => ShortType }
  protected def TINYINT = regex("(?i)TINYINT".r) ^^ {_ => ShortType }
  protected def CHAR = regex("(?i)CHAR".r) ^^ {_ => StringType }
  protected def LONGVARCHAR = regex("(?i)LONGVARCHAR".r) ^^ {_ => StringType }
  protected def VARCHAR = regex("(?i)VARCHAR".r) ^^ {_ => StringType }
  protected def LONGNVARCHAR = regex("(?i)LONGNVARCHAR".r) ^^ {_ => StringType }
  protected def NCHAR = regex("(?i)NCHAR".r) ^^ {_ => StringType }
  protected def BINARY = regex("(?i)BINARY".r) ^^ {_ => BinaryType }
  protected def VARBINARY = regex("(?i)VARBINARY".r) ^^ {_ => BinaryType }
  protected def LONGVARBINARY = regex("(?i)LONGVARBINARY".r) ^^ {_ => BinaryType }
  protected def DATE = regex("(?i)DATE".r) ^^ {_ => DateType }
  protected def TIME = regex("(?i)TIME".r) ^^ {_ => TimeType }
  protected def TIMESTAMP = regex("(?i)TIMESTAMP".r) ^^ {_ => TimestampType }
  protected def CLOB = regex("(?i)CLOB".r) ^^ {_ => ClobType }
  protected def BLOB = regex("(?i)BLOB".r) ^^ {_ => BlobType }
  protected def ARRAY = regex("(?i)ARRAY".r) ^^ {_ => ArrayType }
  protected def UNKNOWN = regex(".*".r) ^^ {_ => UnknownType }

  def parse = {
    BIGINT | DOUBLE | FLOAT | INTEGER | NUMERIC | DECIMAL | REAL | BIT | SMALLINT | TINYINT | CHAR | LONGVARCHAR |
    VARCHAR | LONGNVARCHAR | NCHAR | BINARY | VARBINARY | DATE | TIME | TIMESTAMP | CLOB | BLOB | ARRAY | UNKNOWN
  }

  def apply(value: String) = parseAll(parse, value) match{
    case Success(result:DataType, _) => result
    case _ => UnknownType
  }
}