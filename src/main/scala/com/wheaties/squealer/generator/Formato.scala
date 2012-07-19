package com.wheaties.squealer.generator

trait Formato{
  def tableName(name: String):String
  def columnName(name: String):String
}

object CamelCase extends Formato{

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

object SnakeCase extends Formato{
  def tableName(name: String) = name.split("\\s").map(_.capitalize).foldLeft("")(_ + _)
  def columnName(name: String) = name.split("\\s").map(_.toLowerCase).foldLeft("")(_ + "_" + _)
}

class RegexFormato(regex: String, replaceWith: String) extends Formato{
  def tableName(name: String) = format(name).capitalize
  def columnName(name: String) = format(name)

  protected[squealer] def format(in: String): String = in.replaceAll(regex, replaceWith)
}