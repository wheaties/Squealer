package com.wheaties.squealer.generator.scala

import com.wheaties.squealer.db._
import java.util.Date
import com.wheaties.squealer.generator.Formato

object ScalaDocs extends ((Table,Formato) => List[String]){
  def apply(table: Table, formato: Formato) ={
    val date = new Date(System.currentTimeMillis)
    val creation = formato.tableName(table.name) + " was created on " + date.toString

    val comments = for{
      column <- table.columns
      comment <- column.comment
    } yield{ "@" + formato.columnName(column.name) + " " + comment }

    table.comment match{
      case Some(comment) => comment :: "" :: creation :: "" :: comments
      case None => creation :: "" :: comments
    }
  }
}










