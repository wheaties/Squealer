package com.wheaties.squealer

import config._
import db._
import generator.scala.jdbc.JDBCTree

object Main extends Squealer{
  def main(args: Array[String]){
    if(args.isEmpty){
      action("squealer.conf", FileRecorder)
    }
    else{
      args.foreach(action(_, FileRecorder))
    }
  }
}

/*
 * TODO:
 * 1. Get the validation between SQL statements and DB structure working
 * 2. Get it all working like the pure tables. First step can be empty SQL statements!
 */

trait Squealer{

  def action(configName: String, recorder: Recorder[ParsedResult]) ={
    val ConfigParams(tables, db, format) = ConfigParser(configName)

    val source = ParseDataSource(db.url, db.user, db.password)
    //val results = tables flatMap{ generateTable(_, source, format) }

    //results.map(recorder.record)
  }

  /*def generateTable(table: TableParams, dataSource: Database, format: FormatParams) ={
    val tree = for(dbTable <- dataSource.tables.find(_.name == table.name)) yield JDBCTree(dbTable, table.pack, format)
    tree.map(ParsedResult(table.pack, table.name, _))
  }*/
}