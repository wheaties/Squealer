package com.wheaties.squealer

import com.wheaties.squealer.Implicits._

object Main extends Squealer{
  def main(args: Array[String]){
    if(args.isEmpty){
      action("statements.conf")
    }
    else{
      args.foreach(action)
    }
  }
}

/*
 * TODO:
 * 1. Get SQL parsing working
 * 2. Get the validation between SQL statements and DB structure working
 * 3. Get the column conversions for Nullables (think LEFT JOIN) and aliases (think cf.foo AS bar)
 * 4. Get it all working like the pure tables. First step can be empty SQL statements!
 */

trait Squealer{

  def action(configName: String) ={
    val DatabaseStatements(url, user, password, formato, statements) = ConfigParser(configName)
    val source = ParseDataSource(url, user, password)
    val results = statements flatMap{
      _ match{
        case x:TableStatement => generateTable(x, source, formato)
        case x:ClassStatement => generateClass(x, source)
      }
    }

    results.map(write)
  }

  def generateTable(statement: TableStatement, dataSource: Database, formato: Formato = CamelCase) ={
    val tree = for(table <- dataSource.tables.find(_.name == statement.name)) yield PureTable(table, statement.pack, formato)
    tree.map(ParsedResult(statement.pack, statement.name, _))
  }

  //TODO: finish me once we've got the SQL AST figured out
  def generateClass(statement: ClassStatement, dataSource: Database) ={
    None
  }

  //Really? Am I really making this more unit testable!?
  def write(result: ParsedResult)(implicit recorder: Recorder[ParsedResult]) = recorder.record(result)
}