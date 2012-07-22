package com.wheaties.squealer.generator.scala.squeryl

import treehugger.forest._
import definitions._
import treehuggerDSL._
import com.wheaties.squealer.generator.scala.ScalaDocs
import com.wheaties.squealer.generator.{LibraryGenerator, Formato}
import com.wheaties.squealer.db.{Database, Column, Table}

object SquerylGenerator extends LibraryGenerator{
  def writeTable(table: Table, pack: String, formato: Formato):String ={
    val imports = IMPORT("java.sql._") ::
      IMPORT("javax.sql.rowset.serial.{SerialClob, SerialBlob}") ::
      IMPORT("org.squeryl.PrimitiveTypeMode._") ::
      IMPORT("org.squeryl.annotations.Column") :: Nil
    val comments = ScalaDocs(table, formato)
    val clazz = makeClass(table.name, table.columns, comments, formato)
    val block = imports ::: clazz :: Nil

    val tree = if(pack.isEmpty){
      BLOCK{ block } withoutPackage
    }
    else{
      BLOCK{ block } inPackage(pack)
    }

    treeToString(tree)
  }

  protected[squeryl] def makeClass(name: String, columns: List[Column], comments: List[String], formato: Formato):ClassDef ={
    ConstructorTree(name, columns, formato) := BLOCK{
      DefinitionsTree(columns)
    } withComments(comments)
  }

  def writeDatabase(db: Database, pack: String, formato: Formato):String = {
    val imports = IMPORT("org.squeryl.PrimitiveTypeMode._") ::
      IMPORT("org.squery.Schema") :: Nil
    val obj = OBJECTDEF(formato.databaseName(db.name)) withParents("Schema") := BLOCK{
      db.tables.map(table => tableDef(table.name, formato))
    }
    val block = imports ::: obj :: Nil

    val tree = if(pack.isEmpty){
      BLOCK{ block } withoutPackage
    }
    else{
      BLOCK{ block } inPackage(pack)
    }

    treeToString(tree)
  }

  protected[squeryl] def tableDef(name: String, formato: Formato)={
    VAL(formato.columnName(name)) := REF("table") APPLYTYPE(formato.tableName(name)) APPLY(LIT(name))
  }
}