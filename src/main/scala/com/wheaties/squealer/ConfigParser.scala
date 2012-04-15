package com.wheaties.squealer

import collection.JavaConversions.JListWrapper
import com.typesafe.config.{Config, ConfigFactory}

case class StatementDefinition(pack: String, name: String, statement: String)
case class DatabaseStatements(url: String, user: String, password: String, statements: List[StatementDefinition])

//TODO: add in ability to specify the tables and/or sql statements. Be flexible!
object ConfigParser extends (String => DatabaseStatements){
  val PACKAGE = "package"
  val CLASS_NAME = "class_name"
  val SQL_STATEMENT = "sql_statement"
  val URL = "url"
  val PASSWORD = "password"
  val USER = "user"
  val GROUPS = "groups"
  val TABLES = "tables"

  def apply(fileName: String) = parseConfig(ConfigFactory.load(fileName))

  protected def parseConfig(config: Config) ={
    val statements = for{
      entry <- JListWrapper(config.getConfigList(GROUPS)).toList
      statement <- unpackGroup(entry)
    } yield statement

    val strings = config.getString _
    DatabaseStatements(strings(URL), strings(USER), strings(PASSWORD), statements)
  }

  protected def unpackGroup(config: Config)= try{
    val strings = config.getString _
    Some(StatementDefinition(strings(PACKAGE), strings(CLASS_NAME), strings(SQL_STATEMENT)))
  }
  catch{
    case ex => println(ex.getMessage); None
  }
}