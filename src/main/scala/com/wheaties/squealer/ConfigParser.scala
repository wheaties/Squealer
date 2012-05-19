package com.wheaties.squealer

import collection.JavaConversions.JListWrapper
import com.typesafe.config.{Config, ConfigFactory}

sealed trait Statement{
  val pack: String
  val name: String
}
case class TableStatement(pack: String, name: String) extends Statement
case class ClassStatement(pack: String, name: String, statement: String) extends Statement
case class DatabaseStatements(url: String, user: String, password: String, formato: Formato, statements: List[Statement])

//TODO: wouldn't it be nice to set every table's name and package like trying to do with SQL statements?
object ConfigParser extends (String => DatabaseStatements){
  val PACKAGE = "package"
  val CLASS_NAME = "class_name"
  val SQL_STATEMENT = "sql_statement"
  val URL = "url"
  val PASSWORD = "password"
  val USER = "user"
  val QUERIES = "queries"
  val TABLES = "tables"
  val NAMES = "names"
  val FORMAT = "format"
  val REGEX = "regex"
  val REPLACE_WITH = "replace_with"

  def apply(fileName: String) ={
    val config = ConfigFactory.load(fileName)
    val groups = if(config.hasPath(QUERIES)) parseQueries(config) else Nil
    val tables = if(config.hasPath(TABLES)) parseTables(config.getConfig(TABLES)) else Nil
    val formato = if(config.hasPath(FORMAT)) parseFormat(config) else CamelCase
    val url = config.getString(URL)
    val user = config.getString(USER)
    val password = config.getString(PASSWORD)

    DatabaseStatements(url, user, password, formato, groups ::: tables)
  }

  protected[squealer] def parseQueries(config: Config) = for{
      entry <- JListWrapper(config.getConfigList(QUERIES)).toList
      statement <- unpackQuery(entry)
    } yield statement

  protected[squealer] def unpackQuery(config: Config)= try{
    Some(ClassStatement(config.getString(PACKAGE), config.getString(CLASS_NAME), config.getString(SQL_STATEMENT)))
  }
  catch{
    case ex => println(ex.getMessage); None
  }

  protected[squealer] def parseTables(config: Config) = try{
    val pack = config.getString(PACKAGE)
    for(table <- JListWrapper(config.getStringList(NAMES)).toList) yield TableStatement(pack, table)
  }
  catch{
    case ex => println(ex.getMessage); Nil
  }

  protected[squealer] def parseFormat(config: Config) = try{
    val path = config.getConfig(FORMAT)
    val regex = path.getString(REGEX)
    val replaceWith = path.getString(REPLACE_WITH)

    new RegexFormato(regex, replaceWith)
  }
  catch{
    case ex => println(ex.getMessage); CamelCase
  }
}