package com.wheaties.squealer.generator.scala.jdbc

import com.wheaties.squealer.generator.scala.ScalaDocs
import com.wheaties.squealer.generator.Formato
import treehugger.forest._
import definitions._
import treehuggerDSL._
import com.wheaties.squealer.db.{ColumnNameLens, Column, Table}

//TODO: revisit signature
object JDBCTree extends ((Table,String,Formato) => Tree) {
  def apply(table: Table, pack: String, formato: Formato):Tree ={
    val comments = ScalaDocs(table, formato)
    val columns = table.columns.map(ColumnNameLens.modify(_, formato.columnName))
    val clazz = makeClass(formato.tableName(table.name), columns, comments)
    val objekt = ObjectTree(table.name, table.columns)
    val block = IMPORT("java.sql._") :: clazz :: objekt :: Nil

    if(pack.isEmpty){
      BLOCK{ block } withoutPackage
    }
    else{
      BLOCK{ block } inPackage(pack)
    }
  }

  private[jdbc] def makeClass(name: String, columns: List[Column], comments: List[String]):ClassDef ={
    ConstructorTree(name, columns) := BLOCK{
      AssumptionTree(columns) ::: HashCodeTree(columns) :: EqualsTree(name, columns) ::  CopyTree(name, columns) :: Nil
    } withComments(comments)
  }
}