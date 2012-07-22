package com.wheaties.squealer.generator.scala.format

import com.wheaties.squealer.generator.Formato

class RegexFormato(regex: String, replaceWith: String) extends Formato{
  def databaseName(name: String) = tableName(name)
  def tableName(name: String) = format(name).capitalize
  def columnName(name: String) = format(name)

  protected[squealer] def format(in: String): String = in.replaceAll(regex, replaceWith).replaceAll("\\s", "")
}