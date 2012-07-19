package com.wheaties.squealer.config

import com.typesafe.config.{ConfigObject, Config, ConfigFactory}

object ConfigParser extends (String => ConfigParams){
  val TABLES = "tables"
  val PACKAGE = "package"
  val URL = "url"
  val PASSWORD = "password"
  val USER = "user"
  val NAME = "name"
  val DATABASE = "database"
  val FORMAT = "format"
  val REGEX = "regex"
  val REPLACE_WITH = "replace_with"
  val LANGUAGE = "language"
  val LIBRARY = "library"

  def apply(fileName: String) ={
    val config = ConfigFactory.load(fileName)
    val tables = parseTables(config.getObjectList(TABLES))
    val database = parseDatabase(config.getConfig(DATABASE))
    val format = parseFormat(config.getConfig(FORMAT))

    ConfigParams(tables, database, format)
  }

  protected[squealer] def parseDatabase(config: Config) ={
    val url = config.getString(URL)
    val user = config.getString(USER)
    val password = config.getString(PASSWORD)

    DatabaseParams(url, user, password)
  }

  protected[squealer] def parseTables(config: java.util.List[_ <: ConfigObject]) = try{
    val confIter = config.iterator()
    val builder = List.newBuilder[TableParams]
    while(confIter.hasNext){
      val conf = confIter.next().toConfig
      val name = conf.getString(NAME)
      val pack = conf.getString(PACKAGE)
      builder += TableParams(pack, name)
    }
    builder.result()
  }
  catch{
    case ex => println(ex.getMessage); Nil
  }

  protected[squealer] def parseFormat(config: Config) = try{
    val regex = if(config.hasPath(REGEX)) Some(config.getString(REGEX)) else None
    val replaceWith = if(config.hasPath(REPLACE_WITH)) Some(config.getString(REPLACE_WITH)) else None
    val library = if(config.hasPath(LIBRARY)) Some(config.getString(LIBRARY)) else None
    val language = config.getString(LANGUAGE)

    FormatParams(language, library, regex, replaceWith)
  }
  catch{
    case ex => println(ex.getMessage); FormatParams("scala", None, None, None)
  }
}