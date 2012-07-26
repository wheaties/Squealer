package com.wheaties.squealer.config

case class TableParams(pack: String, names: List[String])
case class DatabaseParams(url: String, user: String, password: String, driver: String)
case class FormatParams(language: String, library: Option[String], regex: Option[String], replacement: Option[String])
case class ConfigParams(table: TableParams, db: DatabaseParams, format: FormatParams)