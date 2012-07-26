package com.wheaties.squealer.config

import com.typesafe.config.{Config, ConfigFactory}
import collection.JavaConversions.JListWrapper
import java.io.{FileReader, FileInputStream, File}

object ConfigParser extends (String => ConfigParams){
  val TABLES = "tables"
  val PACKAGE = "package"
  val URL = "url"
  val PASSWORD = "password"
  val USER = "user"
  val NAMES = "names"
  val DATABASE = "database"
  val DRIVER = "driver"
  val FORMAT = "format"
  val REGEX = "regex"
  val REPLACE_WITH = "replace_with"
  val LANGUAGE = "language"
  val LIBRARY = "library"

  def apply(fileName: String) ={
    val config = ConfigFactory.parseFile(new File(fileName))
    val tables = parseTables(config.getConfig(TABLES))
    val database = parseDatabase(config.getConfig(DATABASE))
    val format = parseFormat(config.getConfig(FORMAT))

    ConfigParams(tables, database, format)
  }

  protected[squealer] def parseDatabase(config: Config) ={
    val url = config.getString(URL)
    val user = config.getString(USER)
    val password = config.getString(PASSWORD)
    val driver = config.getString(DRIVER)

    DatabaseParams(url, user, password, driver)
  }

  protected[squealer] def parseTables(config: Config) = {
    val pack = config.getString(PACKAGE)
    val names = JListWrapper(config.getStringList(NAMES)).toList

    TableParams(pack, names)
  }

  protected[squealer] def parseFormat(config: Config) = {
    val regex = if(config.hasPath(REGEX)) Some(config.getString(REGEX)) else None
    val replaceWith = if(config.hasPath(REPLACE_WITH)) Some(config.getString(REPLACE_WITH)) else None
    val library = if(config.hasPath(LIBRARY)) Some(config.getString(LIBRARY)) else None
    val language = config.getString(LANGUAGE)

    FormatParams(language, library, regex, replaceWith)
  }
}