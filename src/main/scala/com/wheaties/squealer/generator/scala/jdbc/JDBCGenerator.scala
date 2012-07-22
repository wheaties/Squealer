package com.wheaties.squealer.generator.scala.jdbc

import com.wheaties.squealer.generator.scala.ScalaDocs
import treehugger.forest._
import definitions._
import treehuggerDSL._
import com.wheaties.squealer.generator.{LibraryGenerator, Formato}
import com.wheaties.squealer.db.{Database, ColumnNameLens, Column, Table}

object JDBCGenerator extends LibraryGenerator {
  def writeTable(table: Table, pack: String, formato: Formato) ={
    val comments = ScalaDocs(table, formato)
    val columns = table.columns.map(ColumnNameLens.modify(_, formato.columnName))
    val clazz = makeClass(formato.tableName(table.name), columns, comments)
    val objekt = ObjectTree(table.name, table.columns)
    val block = IMPORT("java.sql._") :: clazz :: objekt :: Nil

    val tree = if(pack.isEmpty){
      BLOCK{ block } withoutPackage
    }
    else{
      BLOCK{ block } inPackage(pack)
    }

    treeToString(tree)
  }

  private[jdbc] def makeClass(name: String, columns: List[Column], comments: List[String]):ClassDef ={
    ConstructorTree(name, columns) := BLOCK{
      AssumptionTree(columns) ::: HashCodeTree(columns) :: EqualsTree(name, columns) ::  CopyTree(name, columns) :: Nil
    } withComments(comments)
  }

  def writeDatabase(db: Database, group: String, formato: Formato) = ""
}