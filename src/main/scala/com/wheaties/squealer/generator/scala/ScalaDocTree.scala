package com.wheaties.squealer.generator.scala

import com.wheaties.squealer.db._
import treehugger.forest._
import definitions._
import treehuggerDSL._
import java.util.Date

object ScalaDocTree extends ((String,List[Column]) => (Tree => Tree)){
  def apply(name: String, params: List[Column]) ={
    val date = new Date(System.currentTimeMillis)
    val initial = name + " was created on " + date.toString
    val comments = initial :: "" :: params.flatMap(extractComment)

    def toComments(tree: Tree):Tree = tree withComments(comments)
    toComments _
  }

  private[squealer] def extractComment(column: Column) ={
    for(comment <- column.comment) yield{ "@" + column.name + " " + comment }
  }
}










