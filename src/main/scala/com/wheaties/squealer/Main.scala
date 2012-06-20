package com.wheaties.squealer

import config._
import db._
import generator.{Formato, CamelCase}
import generator.scala.PureTable

object Main extends Squealer{
  def main(args: Array[String]){
    if(args.isEmpty){
      action("statements.conf", FileRecorder)
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
    val DatabaseStatements(url, user, password, formato, statements) = ConfigParser(configName)
    val source = ParseDataSource(url, user, password)
    val results = statements flatMap{
      _ match{
        case x:TableStatement => generateTable(x, source, formato)
        case x:ClassStatement => generateClass(x, source)
      }
    }

    results.map(recorder.record)
  }

  def generateTable(statement: TableStatement, dataSource: Database, formato: Formato = CamelCase) ={
    val tree = for(table <- dataSource.tables.find(_.name == statement.name)) yield PureTable(table, statement.pack, formato)
    tree.map(ParsedResult(statement.pack, statement.name, _))
  }

  //TODO: finish me once we've got the SQL AST figured out
  def generateClass(statement: ClassStatement, dataSource: Database) ={
    None
  }
}