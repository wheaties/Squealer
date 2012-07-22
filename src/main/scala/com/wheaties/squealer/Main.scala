package com.wheaties.squealer

import config._
import db._
import generator.scala.ScalaGenerator

object Main extends Squealer{
  def main(args: Array[String]){
    val exceptions = if(args.isEmpty){
      action(ConfigParser("squealer.conf"))
    }
    else{
      args map(ConfigParser) flatMap(action) toList
    }

    exceptions foreach println
  }
}

trait Squealer{

  def action(params: ConfigParams) ={
    val ConfigParams(tables, db, format) = params

    val source = ParseDataSource(db.url, db.user, db.password)
    val database = DatabaseTableLens.modify(source, _.filter(tables.names contains))
    val generator = ScalaGenerator(format.library, format.regex, format.replacement)

    generator(database, tables.pack)
  }
}