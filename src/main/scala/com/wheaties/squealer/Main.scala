package com.wheaties.squealer

import com.wheaties.squealer.Implicits._

//TODO: By Monday, 16th. Get comments working and make sure pure table works.
object Main extends Squealer{
  def main(args: Array[String]){
    action()
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

  def action() ={
    val DatabaseStatements(url, user, password, statements) = ConfigParser("statements.conf")
    val source = ParseDataSource(url, user, password)
    val results = statements match{
      case Nil => empty(source)
      case _ => nonEmpty(source, statements)
    }
    results.map(write)
  }

  def empty(database: Database) = for{table <- database.tables} yield ParsedResult("", table.name, PureTable(table))

  //TODO: finish me once we've got the SQL AST figured out
  def nonEmpty(database: Database, statements: List[StatementDefinition]) = Nil

  def write(result: ParsedResult)(implicit recorder: Recorder[ParsedResult]) = recorder.record(result)
}

