package com.wheaties.squealer

import collection.JavaConversions.{JMapWrapper, JListWrapper, JSetWrapper}
import com.typesafe.config.{Config, ConfigFactory}

case class StatementDefinition(pack: String, name: String, statement: String)
case class DatabaseStatements(connection: String, user: String, password: String, statements: List[StatementDefinition])

object ConfigParser extends (String => DatabaseStatements){
  val PACKAGE = "package"
  val CLASS_NAME = "class_name"
  val SQL_STATEMENT = "sql_statement"
  val CONNECTION = "connection"
  val PASSWORD = "password"
  val USER = "user"
  val GROUPS = "groups"

  def apply(fileName: String) = {
    val config = ConfigFactory.load(fileName)
    val groups = config.getConfigList(GROUPS)

    val statements = for{
      entry <- JListWrapper(groups).toList
      statement <- unpackGroup(entry)
    } yield statement

    DatabaseStatements(config.getString(CONNECTION), config.getString(USER), config.getString(PASSWORD), statements)
  }

  protected def unpackGroup(config: Config)= try{
    val strings = config.getString _
    Some(StatementDefinition(strings(PACKAGE), strings(CLASS_NAME), strings(SQL_STATEMENT)))
  }
  catch{
    case ex => println(ex.getMessage); None
  }
}