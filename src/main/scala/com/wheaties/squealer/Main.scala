package com.wheaties.squealer

import config._
import db._
import generator.scala.ScalaGenerator

object Main extends Squealer{
  def main(args: Array[String]):Unit = try{
    val exceptions = if(args.isEmpty){
      action(ConfigParser("squealer.conf"))
    }
    else{
      args map(ConfigParser) flatMap(action) toList
    }

    exceptions foreach println
  }
  catch{
    case ex:Exception => println(ex)
  }
}

trait Squealer{

  def action(params: ConfigParams) ={
    val ConfigParams(tables, db, format) = params
    val DatabaseParams(url, user, password, driver) = db

    val source = ParseDataSource(url, user, password, driver)
    val database = DatabaseTableLens modify(source, _.filter(tables.names contains))
    val generator = ScalaGenerator(format.library, format.regex, format.replacement)

    generator(database, tables.pack)
  }
}