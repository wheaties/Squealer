package com.wheaties.squealer

import treehugger.forest._
import java.io.{File, OutputStream}

//TODO: should this be the main? So much logic lives here...
class Main {
  def main(args: Array[String]) ={
    val DatabaseStatements(url, user, password, statements) = ConfigParser("statements.conf")
    val source = ParseDataSource(url, user, password)
    val results = statements match{
      case Nil => empty(source)
      case _ => nonEmpty(source, statements)
    }
  }

  def empty(database: Database) = for{table <- database.tables} yield ParsedResult("", table.name, PureTable(table))

  //TODO: finish me once we've got the SQL AST figured out
  def nonEmpty(database: Database, statements: List[StatementDefinition]) = Nil

  def write(result: ParsedResult) ={
    val file = new File("TODO: finish me")
  }
}

case class ParsedResult(classPackage: String, className: String, ast: Tree)