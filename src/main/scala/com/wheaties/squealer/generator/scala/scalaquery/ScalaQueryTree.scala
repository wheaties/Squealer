package com.wheaties.squealer.generator.scala.scalaquery

import com.wheaties.squealer.db.Table
import com.wheaties.squealer.generator.Formato
import treehugger.forest._
import definitions._
import treehuggerDSL._
import com.wheaties.squealer.generator.scala.ScalaDocs

class ScalaQueryTree {
  def apply(table: Table, pack: String, formato: Formato):Tree ={
    val imports = IMPORT("java.sql._") ::
      IMPORT("org.scalaquery.ql.basic.BasicTable") ::
      IMPORT("org.scalaquery.ql.TypeMapper._") ::
      IMPORT("org.scalaquery.ql._") :: Nil
    val comments = ScalaDocs(table, formato.format)
    val objekt = ObjectTree(table.name, table.columns, formato.format) withComments(comments)
    val block =  imports ::: objekt :: Nil

    if(pack.isEmpty){
      BLOCK{ block } withoutPackage
    }
    else{
      BLOCK{ block } inPackage(pack)
    }
  }
}