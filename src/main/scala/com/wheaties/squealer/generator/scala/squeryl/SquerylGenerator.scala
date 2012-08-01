/*
Copyright (c) 2012 Owein Reese

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
documentation files (the "Software"), to deal in the Software without restriction, including without limitation the
rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit
persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the
Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

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
    val clazz = makeClass(table.name, table.columns, formato) withComments(comments)
    val block = imports ::: clazz :: Nil

    val tree = if(pack.isEmpty){
      BLOCK{ block } withoutPackage
    }
    else{
      BLOCK{ block } inPackage(pack)
    }

    treeToString(tree)
  }

  protected[squeryl] def makeClass(name: String, columns: List[Column], formato: Formato):ClassDef =
    ConstructorTree(name, columns, formato) := BLOCK{ DefinitionsTree(columns) }

  def writeDatabase(db: Database, pack: String, formato: Formato):String = {
    val imports = IMPORT("org.squeryl.PrimitiveTypeMode._") :: IMPORT("org.squery.Schema") :: Nil
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