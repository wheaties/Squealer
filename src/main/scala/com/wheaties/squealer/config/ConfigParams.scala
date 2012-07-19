package com.wheaties.squealer.config

case class TableParams(pack: String, name: String)
case class DatabaseParams(url: String, user: String, password: String)
case class FormatParams(language: String, library: Option[String], regex: Option[String], replacement: Option[String])
case class ConfigParams(tables: List[TableParams], db: DatabaseParams, format: FormatParams)