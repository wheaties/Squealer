package com.wheaties.squealer.generator.scala.squeryl

import com.wheaties.squealer.generator.Formato
import treehugger.forest._
import definitions._
import treehuggerDSL._
import com.wheaties.squealer.generator.scala.ScalaDocs
import com.wheaties.squealer.db.{Column, Table}

//TODO: this changes things. Squeryl will require one more file than tables produced. Need to move the production into each package!
object SquerylTree extends ((Table,String,Formato) => Tree){
  def apply(table: Table, pack: String, formato: Formato):Tree ={
    val imports = IMPORT("java.sql._") ::
      IMPORT("javax.sql.rowset.serial.SerialClob") ::
      IMPORT("javax.sql.rowset.serial.SerialBlob") ::
      IMPORT("org.squeryl.PrimitiveTypeMode._") ::
      IMPORT("org.squery.Schema") ::
      IMPORT("org.squeryl.annotations.Column") :: Nil
    val comments = ScalaDocs(table, formato)
    val clazz = makeClass(table.name, table.columns, comments, formato)
    val block = imports ::: clazz :: Nil

    if(pack.isEmpty){
      BLOCK{ block } withoutPackage
    }
    else{
      BLOCK{ block } inPackage(pack)
    }
  }

  private[squeryl] def makeClass(name: String, columns: List[Column], comments: List[String], formato: Formato):ClassDef ={
    ConstructorTree(name, columns, formato) := BLOCK{
      DefinitionsTree(columns)
    } withComments(comments)
  }
}