package com.wheaties.squealer.generator.scala

import com.wheaties.squealer.db._
import java.util.Date

object ScalaDocs extends ((Table,String => String) => List[String]){
  def apply(table: Table, formatto: String => String) ={
    val date = new Date(System.currentTimeMillis)
    val creation = formatto(table.name).capitalize + " was created on " + date.toString

    val comments = for{
      column <- table.columns
      comment <- column.comment
    } yield{ "@" + formatto(column.name) + " " + comment }

    table.comment match{
      case Some(comment) => comment :: "" :: creation :: "" :: comments
      case None => creation :: "" :: comments
    }
  }
}










