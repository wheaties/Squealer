package com.wheaties.squealer.generator.scala.scalaquery

import treehugger.forest._
import definitions._
import treehuggerDSL._
import com.wheaties.squealer.generator.scala.ScalaDocs
import com.wheaties.squealer.generator.{LibraryGenerator, Formato}
import com.wheaties.squealer.db.{Database, Table}

object ScalaQueryGenerator extends LibraryGenerator{
  def writeTable(table: Table, pack: String, formato: Formato) ={
    val imports = IMPORT("java.sql._") ::
      IMPORT("org.scalaquery.ql.basic.BasicTable") ::
      IMPORT("org.scalaquery.ql.TypeMapper._") ::
      IMPORT("org.scalaquery.ql._") :: Nil
    val comments = ScalaDocs(table, formato)
    val objekt = ObjectTree(table.name, table.columns, formato) withComments(comments)
    val block =  imports ::: objekt :: Nil

    val tree = if(pack.isEmpty){
      BLOCK{ block } withoutPackage
    }
    else{
      BLOCK{ block } inPackage(pack)
    }

    treeToString(tree)
  }

  def writeDatabase(db: Database, group: String, formato: Formato) = ""
}