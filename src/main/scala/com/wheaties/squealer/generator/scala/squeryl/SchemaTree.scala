package com.wheaties.squealer.generator.scala.squeryl

import treehugger.forest._
import definitions._
import treehuggerDSL._

object SchemaTree extends ((String, List[String], String => String) => Tree){
  def apply(dbName: String, tableNames: List[String], formato: String => String) ={
    OBJECTDEF(formato(dbName).capitalize) withParents("Schema") := BLOCK{
      tableNames.map(tableDef(_, formato))
    }
  }

  protected[squeryl] def tableDef(name: String, formato: String => String)={
    VAL(formato(name)) := REF("table") APPLYTYPE(formato(name).capitalize) APPLY(LIT(name))
  }
}