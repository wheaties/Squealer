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

package com.wheaties.squealer.generator.scala

import com.wheaties.squealer.db.{Database, Table}
import format.{RegexFormato, CamelCase}
import java.io.{FileWriter, File}
import com.wheaties.squealer.generator._
import annotation.tailrec
import jdbc.JDBCGenerator
import scalaquery.ScalaQueryGenerator
import squeryl.SquerylGenerator

//TODO: This is so horribly OO. Really need to think about type classes for the libraries/language combo
class ScalaGenerator(formato: Formato, gen: LibraryGenerator) extends ((Database,String) => List[Exception]){
  protected[generator] val basePath = "src" + File.separator + "main" + File.separator + "scala"

  def apply(db: Database, pack: String) = {
    @tailrec def rollback(in: List[Result[Exception,File]], reasons: List[Exception]): List[Exception] = in match {
      case Success(file:File) :: rest => file.delete(); rollback(rest, reasons)
      case Failure(ex) :: rest => rollback(rest, ex :: reasons)
      case Nil => reasons
    }

    val results = writeDatabase(db, pack) :: db.tables.map(writeTable(_, pack))
    if(results.exists(_.isInstanceOf[Failure[Exception,File]])){
      rollback(results, Nil)
    }
    else{
      Nil
    }
  }

  protected[generator] def writeDatabase(db: Database, pack: String) = {
    val output = gen.writeDatabase(db, pack, formato)

    if(!output.isEmpty){
      writeFile(formato.databaseName(db.name), pack, output)
    }
    else{
      Success(new File(""){ override def delete() = true })
    }
  }

  protected[generator] def writeTable(table: Table, pack: String) = {
    val output = gen.writeTable(table, pack, formato)

    writeFile(formato.tableName(table.name), pack, output)
  }

  protected[generator] def writeFile(name: String, pack: String, output: String): Result[Exception,File] = {
    val path = basePath + File.separator + pack.replaceAll("\\.", File.separator)
    val fileName = path + File.separator + name + ".scala"

    try{
      val dir = new File(path)
      if(!dir.exists){
        dir.mkdirs()
      }
      val file = new File(fileName)
      if(!file.exists){
        file.createNewFile()
      }
      val writer = new FileWriter(file)
      writer.write(output)
      writer.close()
      Success(file)
    }
    catch{
      case ex: Exception => Failure(ex)
    }
  }
}

object ScalaGenerator{

  protected val squeryl = """(?i)squeryl""".r
  protected val scalaquery = """(?i)scalaquery""".r

  def apply(library: Option[String], regex: Option[String], replaceWith: Option[String])={
    val formato = makeFormato(regex, replaceWith)

    library match{
      case Some(squeryl(_)) => new ScalaGenerator(formato, SquerylGenerator)
      case Some(scalaquery(_)) => new ScalaGenerator(formato, ScalaQueryGenerator)
      case _ => new ScalaGenerator(formato, JDBCGenerator)
    }
  }

  protected[scala] def makeFormato(regex: Option[String], replaceWith: Option[String]) ={
    val formato = for{
      rgx <- regex
      replace <- replaceWith
    } yield new RegexFormato(rgx, replace)

    formato getOrElse CamelCase
  }
}