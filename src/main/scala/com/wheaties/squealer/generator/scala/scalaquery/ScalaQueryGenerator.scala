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