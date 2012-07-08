package com.wheaties.squealer.generator

import com.wheaties.squealer.db._

//trait Formato[Obj] extends (Obj => Obj){
//  def apply(obj: Obj):Obj
//
//  def format(in: String): String
//}

trait Formato extends (Table => Table){

  def apply(table: Table)={
    val Table(name, comment, columns) = table

    Table(format(name).capitalize, comment, columns.map(ColumnNameLens.modify(_, format)))
  }

  protected[squealer] def format(name: String):String
}

object CamelCase extends Formato{
  /**
   *  Change "HelloWorld" to "helloWorld"
   *  Change "Hello_World" to "helloWorld"
   */
  protected[squealer] def format(in: String): String = in.split("[_\\s]").toList match{
    case word :: Nil if word.nonEmpty => word.charAt(0).toLower + word.drop(1)
    case firstWord :: rest => firstWord.toLowerCase + rest.map(_.capitalize).mkString
    case Nil => ""
  }
}

class RegexFormato(regex: String, replaceWith: String) extends Formato{
  protected[squealer] def format(in: String): String ={
    val word = in.replaceAll(regex, replaceWith)
    word.charAt(0).toLower + word.drop(1)
  }
}