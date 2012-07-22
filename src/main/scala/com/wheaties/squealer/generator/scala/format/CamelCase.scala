package com.wheaties.squealer.generator.scala.format

import com.wheaties.squealer.generator.Formato

object CamelCase extends Formato{

  def databaseName(name: String) = tableName(name)
  def tableName(name: String) = format(name).capitalize
  def columnName(name: String) = format(name)

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