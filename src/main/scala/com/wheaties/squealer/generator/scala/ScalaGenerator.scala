package com.wheaties.squealer.generator.scala

import com.wheaties.squealer.db.{Database, Table}
import format.{RegexFormato, CamelCase}
import java.io.{FileWriter, File}
import com.wheaties.squealer.generator._
import annotation.tailrec
import jdbc.JDBCGenerator
import scalaquery.ScalaQueryGenerator
import squeryl.SquerylGenerator

class ScalaGenerator(formato: Formato, gen: LibraryGenerator) extends ((Database,List[Table],String) => List[Exception]){
  protected[generator] val basePath = "src" + File.pathSeparator + "main" + File.pathSeparator + "scala"

  def apply(db: Database, tables: List[Table], pack: String) = {
    @tailrec def rollback(in: List[Result[Exception,File]], reasons: List[Exception]): List[Exception] = in match {
      case Success(file:File) :: rest => file.delete(); rollback(rest, reasons)
      case Failure(ex) :: rest => rollback(rest, ex :: reasons)
      case Nil => reasons
    }

    val results = writeDatabase(db.copy(tables = tables), pack) :: tables.map(writeTable(_, pack))
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
    val path = pack.replaceAll("\\.", File.pathSeparator)
    val fileName = basePath + File.pathSeparator + path + File.pathSeparator + name + ".scala"

    try{
      val file = new File(fileName)
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

  def apply(library: String, regex: Option[String], replaceWith: Option[String])={
    val formato = makeFormato(regex, replaceWith)

    library match{
      case squeryl(_) => new ScalaGenerator(formato, SquerylGenerator)
      case scalaquery(_) => new ScalaGenerator(formato, ScalaQueryGenerator)
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