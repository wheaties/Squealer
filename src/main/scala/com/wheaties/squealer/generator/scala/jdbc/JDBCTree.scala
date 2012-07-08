package com.wheaties.squealer.generator.scala.jdbc

import com.wheaties.squealer.db.{Column, Table}
import com.wheaties.squealer.generator.scala.ScalaDocs
import com.wheaties.squealer.generator.Formato
import treehugger.forest._
import definitions._
import treehuggerDSL._

//TODO: revisit signature
object JDBCTree extends ((Table,String,Formato) => Tree) {
  def apply(table: Table, pack: String, formatter: Formato):Tree ={
    val formattedTable = formatter(table)
    val comments = ScalaDocs(table, formatter.format)
    val clazz = makeClass(formattedTable.name, formattedTable.columns, comments)
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